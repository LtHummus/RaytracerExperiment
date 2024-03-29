package com.lthummus.raytracer.parsers.scene

import java.io.File

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.parsers.ObjFile
import com.lthummus.raytracer.pattern.{CheckerPattern, GradientPattern, Pattern, RingPattern, StripedPattern}
import com.lthummus.raytracer.primitive.{Color, Matrix, Point, Tuple, Vec}
import com.lthummus.raytracer.shapes.{Cube, Plane, Shape, Sphere}
import com.lthummus.raytracer.tools.{RotateX, RotateY, RotateZ, Scale, Sheer, Translate, ViewTransformation}
import com.typesafe.scalalogging.Logger
import io.circe.{Decoder, DecodingFailure, Json}
import io.circe.generic.JsonCodec
import cats.implicits._

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

sealed trait SceneInput {
  val kind: String
}

trait Shapeable {
  protected def getMaterial(name: Option[String], materials: mutable.HashMap[String, SimpleMaterial]): SimpleMaterial = {
    name match {
      case None =>
        SceneInput.Log.warn("No material name specified. Using default material")
        SimpleMaterial.Default
      case Some(realMaterialName) =>
        materials.getOrElse(realMaterialName,  { SceneInput.Log.warn(s"$realMaterialName not found. Using default material"); SimpleMaterial.Default } )
    }
  }

  def asShape(materials: mutable.HashMap[String, SimpleMaterial]) : Either[String, Shape]
}

trait Transformable {
  val transforms: Option[Seq[Transform]]

  protected def generateTransform: Matrix = {
    transforms match {
      case None                => Matrix.Identity4
      case Some(transformList) => transformList.map(_.asMatrix).foldRight(Matrix.Identity4)(_ * _)
    }
  }
}

@JsonCodec case class Camera(kind: String, height: Int, width: Int, fieldOfView: Double, location: Seq[Double], pointing: Seq[Double], up: Seq[Double]) extends SceneInput {
  def asSimpleCamera: SimpleCamera = SimpleCamera(height, width, fieldOfView, ViewTransformation(Vec(location), Vec(pointing), Vec(up)))
}
@JsonCodec case class Material(kind: String,
                               name: String,
                               color: Seq[Double],
                               ambient: Double,
                               diffuse: Double,
                               specular: Double,
                               shininess: Double,
                               reflective: Double,
                               transparency: Double,
                               refractiveIndex: Double,
                               pattern: Option[PatternInput]) extends SceneInput {
  def asSimpleMaterial: SimpleMaterial = SimpleMaterial(Color(color), ambient, diffuse, specular, shininess, reflective, transparency, refractiveIndex, pattern.flatMap(_.asPattern))
}

@JsonCodec case class Primitive(kind: String, shape: String, material: Option[String], transforms: Option[Seq[Transform]]) extends SceneInput with Transformable with Shapeable {
  //TODO: this should probably be a level above so we can apply materials properly
  override def asShape(materials: mutable.HashMap[String, SimpleMaterial]): Either[String, Shape] = {
    val m = getMaterial(material, materials)
    shape match {
      case "sphere"  => Right(Sphere(generateTransform, m))
      case "cube"    => Right(Cube(generateTransform, m))
      case "plane"   => Right(Plane(generateTransform, m))
      case s: String => Left(s"Unknown shape: $s")
    }
  }
}

@JsonCodec case class Mesh(kind: String, source: Option[String], contents: Option[String], material: Option[String], transforms: Option[Seq[Transform]]) extends SceneInput with Transformable with Shapeable {
  override def asShape(materials: mutable.HashMap[String, SimpleMaterial]): Either[String, Shape] = {
    val m = getMaterial(material, materials)

    val realContents = (source, contents) match {
      case (_, Some(content)) => Try(ObjFile.fromRawString(content)).toEither.leftMap(t => s"${t.getClass.getSimpleName} -- ${t.getMessage}")
      case (Some(f), _) => Try(ObjFile.fromFile(new File(f))).toEither.leftMap(t => s"${t.getClass.getSimpleName} -- ${t.getMessage}")
      case _ => Left("Mesh doesn't have contents or path specified")
    }

    realContents.map { res =>
      res.parentGroup.copy(transformation = generateTransform).setMaterial(m)
    }
  }
}
@JsonCodec case class Light(kind: String, shape: String, pos: Seq[Double], color: Seq[Double]) extends SceneInput {
  def asLight: PointLight = PointLight(Point(pos), Color(color))
}

@JsonCodec case class WorldInfo(kind: String, bgColor: Seq[Double]) extends SceneInput

@JsonCodec case class Transform(kind: String, arguments: Seq[Double]) {
  import Transform._
  def asMatrix: Matrix = {
    kind match {
      case "translate" => handleMatrixErrors("translate", Try(Translate(arguments(0), arguments(1), arguments(2))))
      case "scale"     => handleMatrixErrors("scale", Try(Scale(arguments(0), arguments(1), arguments(2))))
      case "rotateX"   => handleMatrixErrors("rotateX", Try(RotateX(arguments(0))))
      case "rotateY"   => handleMatrixErrors("rotateY", Try(RotateY(arguments(0))))
      case "rotateZ"   => handleMatrixErrors("rotateZ", Try(RotateZ(arguments(0))))
      case "sheer"     => handleMatrixErrors("sheer", Try(Sheer(arguments(0), arguments(1), arguments(2), arguments(3), arguments(4), arguments(5))))
      case s: String   => SceneInput.Log.warn(s"Unknown translation kind: $s"); Matrix.Identity4
    }
  }
}

object Transform {
  private val Log = Logger("Transform")
  private def handleMatrixErrors(kind: String, potentialTransform: Try[Matrix]): Matrix = {
    potentialTransform match {
      case Failure(_)     => Log.warn(s"Unable to generate $kind transformation matrix. Ignoring"); Matrix.Identity4
      case Success(value) => value
    }
  }
}

@JsonCodec case class PatternInput(kind: String, colorA: Seq[Double], colorB: Seq[Double], transforms: Option[Seq[Transform]]) extends Transformable {
  def asPattern: Option[Pattern] = kind match {
    case "checker"  => Some(CheckerPattern(Color(colorA), Color(colorB), generateTransform))
    case "gradient" => Some(GradientPattern(Color(colorA), Color(colorB), generateTransform))
    case "ring"     => Some(RingPattern(Color(colorA), Color(colorB), generateTransform))
    case "striped"  => Some(StripedPattern(Color(colorA), Color(colorB), generateTransform))
    case s: String  => SceneInput.Log.warn(s"Unknown pattern kind: $s"); None
  }
}

object SceneInput {
  private[scene] val Log = Logger("SceneInput")

  def decode(json: Json): Decoder.Result[SceneInput] = {
    (json \\ "kind").head.as[String] match {
      case Right("camera")    => json.as[Camera]
      case Right("primitive") => json.as[Primitive]
      case Right("material")  => json.as[Material]
      case Right("light")     => json.as[Light]
      case Right("mesh")      => json.as[Mesh]
      case Right("world")     => json.as[WorldInfo]
      case Right(x)           => Left(DecodingFailure(s"Unknown kind: $x", List()))
      case Left(x)            => Left(DecodingFailure(s"Failed to decode: ${x.getMessage()}", List()))
    }
  }
}