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

  val shapes = mutable.ArrayBuffer.empty[Shape]
  val materials = mutable.HashMap.empty[String, SimpleMaterial]
  var camera: Option[SimpleCamera] = None
  var light: Option[PointLight] = None

  yaml.parser.parse(data) match {
    case Left(error) => Log.warn(s"Error parsing scene file: $error"); throw new Exception(error.message)
    case Right(parsedTree) =>
      val c = parsedTree.asArray
      val everything = c.get.map(SceneInput.decode)


      everything.foreach {
        case Left(error)                 => Log.warn(s"Error: $error")
        case Right(sceneObject: Camera)  => camera = Some(sceneObject.asSimpleCamera)
        case Right(primitive: Primitive) => shapes += primitive.asShape(materials)
        case Right(mesh: Mesh)           => shapes += mesh.asShape
        case Right(sceneLight: Light)    => light = Some(sceneLight.asLight)
        case Right(material: Material)   => materials.put(material.name, material.asSimpleMaterial)
        case _                           => //nop
      }

      if (camera.isEmpty)
        Log.warn("No camera defined in file!")

      if (light.isEmpty)
        Log.warn("No light defined in file!")
  }
}

object SceneBuilder {
  private val Log: Logger = Logger("HummusSceneBuilder")
}