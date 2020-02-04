package com.lthummus.raytracer.parsers.scene

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.primitive.{Color, Matrix, Point, Tuple, Vec}
import com.lthummus.raytracer.shapes.{Cube, Plane, Shape, Sphere}
import com.lthummus.raytracer.tools.{RotateX, RotateY, RotateZ, Scale, Sheer, Translate, ViewTransformation}
import io.circe.{Decoder, DecodingFailure, Json}
import io.circe.generic.JsonCodec

sealed trait SceneInput {
  val kind: String
}

@JsonCodec case class Camera(kind: String, height: Int, width: Int, fieldOfView: Double, location: Seq[Double], pointing: Seq[Double], up: Seq[Double]) extends SceneInput {
  def asSimpleCamera: SimpleCamera = SimpleCamera(height, width, fieldOfView, ViewTransformation(Vec(location), Vec(pointing), Vec(up)))
}
@JsonCodec case class Material(kind: String, color: Seq[Double]) extends SceneInput
@JsonCodec case class Primitive(kind: String, shape: String, transforms: Option[Seq[Transform]]) extends SceneInput {
  private def generateTransform: Matrix = {
    transforms match {
      case None                => Matrix.Identity4
      case Some(transformList) => transformList.map(_.asMatrix).foldRight(Matrix.Identity4)(_ * _)
    }
  }

  //TODO: this should probably be a level above so we can apply materials properly
  def asShape: Shape = {
    shape match {
      case "sphere" => Sphere(generateTransform)
      case "cube"   => Cube(generateTransform)
      case "plane"  => Plane(generateTransform)
    }
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
      case _           => throw new IllegalArgumentException(s"unknown transform: $kind")
    }
  }
}

object SceneInput {
  def decode(json: Json): Decoder.Result[SceneInput] = {
    (json \\ "kind").head.as[String] match {
      case Right("camera")    => json.as[Camera]
      case Right("primitive") => json.as[Primitive]
      case Right("material")  => json.as[Material]
      case Right("light")     => json.as[Light]
      case _                  => Left(DecodingFailure("Unknown kind", List()))
    }
  }
}