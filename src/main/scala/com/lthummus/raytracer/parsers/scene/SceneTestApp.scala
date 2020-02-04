package com.lthummus.raytracer.parsers.scene

import java.io.File

import com.lthummus.raytracer.world.World
import javax.imageio.ImageIO

object SceneTestApp extends App {

  val f = new File("/Users/bschwartz/test2.yaml")
  val data = scala.io.Source.fromFile(f).getLines().toSeq.mkString("\n")
  val scene = HummusScene.fromRawText(data)

  val world = World.create(scene.objects, scene.light)
  val render = scene.camera.render(world)

  ImageIO.write(render.asBufferedImage, "PNG", new File("t3.png"))
}
