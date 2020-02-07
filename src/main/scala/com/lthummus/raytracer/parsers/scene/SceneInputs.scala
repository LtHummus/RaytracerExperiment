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

import scala.collection.mutable

sealed trait SceneInput {
  val kind: String
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

@JsonCodec case class Primitive(kind: String, shape: String, material: Option[String], transforms: Option[Seq[Transform]]) extends SceneInput with Transformable {
  //TODO: this should probably be a level above so we can apply materials properly
  def asShape(materials: mutable.HashMap[String, SimpleMaterial]): Shape = {
    val m = (for {
      realMatName <- material
      realMaterial <- materials.get(realMatName)
    } yield realMaterial).getOrElse(SimpleMaterial.Default)
    shape match {
      case "sphere"  => Sphere(generateTransform, m)
      case "cube"    => Cube(generateTransform, m)
      case "plane"   => Plane(generateTransform, m)
      case s: String => SceneInput.Log.warn(s"Unknown shape: $s"); ??? //TODO: make optional
    }
  }
}

@JsonCodec case class Mesh(kind: String, source: String, transforms: Option[Seq[Transform]]) extends SceneInput with Transformable {
  def asShape: Shape = {
    ObjFile.fromFile(new File(source)).parentGroup.copy(transformation = generateTransform)
  }
}
@JsonCodec case class Light(kind: String, shape: String, pos: Seq[Double], color: Seq[Double]) extends SceneInput {
  def asLight: PointLight = PointLight(Point(pos), Color(color))
}

@JsonCodec case class Transform(kind: String, arguments: Seq[Double]) {
  def asMatrix: Matrix = {
    kind match {
      case "translate" => Translate(arguments(0), arguments(1), arguments(2))
      case "scale"     => Scale(arguments(0), arguments(1), arguments(2))
      case "rotateX"   => RotateX(arguments(0))
      case "rotateY"   => RotateY(arguments(0))
      case "rotateZ"   => RotateZ(arguments(0))
      case "sheer"     => Sheer(arguments(0), arguments(1), arguments(2), arguments(3), arguments(4), arguments(5))
      case s: String   => SceneInput.Log.warn(s"Unknown translation kind: $s"); Matrix.Identity4
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
      case Right(x)           => Left(DecodingFailure(s"Unknown kind: $x", List()))
      case Left(x)            => Left(DecodingFailure(s"Failed to decode: ${x.getMessage()}", List()))
    }
  }
}