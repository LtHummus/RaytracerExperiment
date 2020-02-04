package com.lthummus.raytracer.parsers.scene

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.shapes.Shape

case class HummusScene(objects: Seq[Shape], light: PointLight, camera: SimpleCamera)

object HummusScene {
  def fromRawText(text: String): HummusScene = {
    val builder = new HummusSceneBuilder(text)

    HummusScene.fromBuilder(builder)
  }

  private def fromBuilder(builder: HummusSceneBuilder): HummusScene = {
    HummusScene(builder.shapes.toSeq, builder.light.get, builder.camera.get)
  }
}
