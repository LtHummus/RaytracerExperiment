package com.lthummus.raytracer.parsers.scene

import java.io.File

import com.lthummus.raytracer.world.World
import javax.imageio.ImageIO

object SceneTestApp extends App {

  val f = new File(args(0))
  val source = scala.io.Source.fromFile(f)
  val lines = source.getLines().toSeq.mkString("\n")
  source.close()
  val scene = HummusScene.fromRawText(lines)

  val world = World.create(scene.objects, scene.light)
  val render = scene.camera.render(world)

  ImageIO.write(render.asBufferedImage, "PNG", new File("t3.png"))
}
