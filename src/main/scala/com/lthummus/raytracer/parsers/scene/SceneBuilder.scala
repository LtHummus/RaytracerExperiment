package com.lthummus.raytracer.parsers.scene

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.Color
import com.lthummus.raytracer.shapes.Shape
import com.lthummus.raytracer.world.World
import com.typesafe.scalalogging.Logger
import io.circe.yaml

import scala.collection.mutable


private[scene] class SceneBuilder(data: String) {
  import SceneBuilder._

  private[scene] val shapes = mutable.ArrayBuffer.empty[Shape]
  private[scene] val materials = mutable.HashMap.empty[String, SimpleMaterial]
  private[scene] var camera: Option[SimpleCamera] = None
  private[scene] var lights = mutable.ArrayBuffer.empty[PointLight]
  private[scene] var errored = false
  private[scene] var worldInfo: Option[WorldInfo] = None

  yaml.parser.parse(data) match {
    case Left(error) => Log.warn(s"Error parsing scene file: $error"); throw new Exception(error.message)
    case Right(parsedTree) =>
      val c = parsedTree.asArray
      val everything = c.get.map(SceneInput.decode)

      everything.foreach {
        case Left(error)                 => Log.warn(s"Error: ${error.message} -- ${error.history.mkString(" -> ")}"); errored = true
        case Right(sceneObject: Camera)  => camera = Some(sceneObject.asSimpleCamera)
        case Right(primitive: Primitive) => shapes += primitive.asShape(materials)
        case Right(mesh: Mesh)           => shapes += mesh.asShape(materials)
        case Right(sceneLight: Light)    => lights += sceneLight.asLight
        case Right(material: Material)   => materials.put(material.name, material.asSimpleMaterial)
        case Right(world: WorldInfo)     => worldInfo = Some(world)
        case _                           => //nop
      }

      if (camera.isEmpty) {
        errored = true
        Log.warn("No camera defined in file!")
      }

      if (lights.isEmpty) {
        errored = true
        Log.warn("No light defined in file!")
      }
  }
}

object SceneBuilder {
  private val Log: Logger = Logger("SceneBuilder")
}
