package com.lthummus.raytracer.parsers.scene

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.shapes.Shape

case class ParsedScene(objects: Seq[Shape], light: PointLight, camera: SimpleCamera)

object ParsedScene {
  def fromRawText(text: String): ParsedScene = {
    val builder = new SceneBuilder(text)

    ParsedScene.fromBuilder(builder)
  }

  private def fromBuilder(builder: SceneBuilder): ParsedScene = {
    ParsedScene(builder.shapes.toSeq, builder.light.get, builder.camera.get)
  }
}