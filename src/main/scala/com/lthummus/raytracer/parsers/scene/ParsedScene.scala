package com.lthummus.raytracer.parsers.scene

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.primitive.Color
import com.lthummus.raytracer.shapes.Shape
import com.lthummus.raytracer.world.World

case class ParsedScene(objects: Seq[Shape], lights: Seq[PointLight], camera: SimpleCamera, worldInfo: Option[WorldInfo]) {
  def toWorld: World = {
    worldInfo match {
      //todo: clean up the logic behind creating...it's because you can't overload methods with default arguments...
      case None       => World.createWithMultipleLights(objects, lights)
      case Some(info) => World.createWithMultipleLights(objects, lights, Color(info.bgColor))
    }
  }
}

object ParsedScene {
  def fromRawText(text: String): Either[Seq[String], ParsedScene] = {
    val builder = new SceneBuilder(text)

    ParsedScene.fromBuilder(builder)
  }

  private def fromBuilder(builder: SceneBuilder): Either[Seq[String], ParsedScene] = builder.asScene

}
