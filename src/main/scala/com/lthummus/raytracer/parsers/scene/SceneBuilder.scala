package com.lthummus.raytracer.parsers.scene

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.shapes.Shape
import com.typesafe.scalalogging.Logger
import io.circe.yaml


import scala.collection.mutable


private[scene] class SceneBuilder(data: String) {
  import SceneBuilder._

  private[scene] val shapes = mutable.ArrayBuffer.empty[Shape]
  private[scene] val materials = mutable.HashMap.empty[String, SimpleMaterial]
  private[scene] var camera: Option[SimpleCamera] = None
  private[scene] var lights = mutable.ArrayBuffer.empty[PointLight]
  private[scene] val errors = mutable.ArrayBuffer.empty[String]
  private[scene] var worldInfo: Option[WorldInfo] = None

  yaml.parser.parse(data) match {
    case Left(error) => Log.warn(s"Error parsing scene file: $error"); throw new Exception(error.message)
    case Right(parsedTree) =>
      val c = parsedTree.asArray
      val everything = c.get.map(SceneInput.decode)

      everything.foreach {
        case Left(error)                 => errors += s"${error.getMessage()} ${error.history.mkString(", ")}"
        case Right(sceneObject: Camera)  => camera = Some(sceneObject.asSimpleCamera)
        case Right(primitive: Primitive) => handleParsedItem(primitive)
        case Right(mesh: Mesh)           => handleParsedItem(mesh)
        case Right(sceneLight: Light)    => lights += sceneLight.asLight
        case Right(material: Material)   => materials.put(material.name, material.asSimpleMaterial)
        case Right(world: WorldInfo)     => worldInfo = Some(world)
        case _                           => //nop
      }
  }

  private def handleParsedItem(item: Shapeable): Unit = {
    item.asShape(materials) match {
      case Right(s)    => shapes += s
      case Left(error) => errors += error
    }
  }

  def asScene: Either[Seq[String], ParsedScene] = {
    if (camera.isEmpty)
      errors += "No camera defined in scene"

    if (lights.isEmpty)
      Log.warn("No lights defined in scene")

    if (errors.isEmpty) {
      Right(ParsedScene(shapes.toSeq, lights.toSeq, camera.get, worldInfo))
    } else {
      Left(errors.toSeq)
    }
  }
}

object SceneBuilder {
  private val Log: Logger = Logger("SceneBuilder")
}
