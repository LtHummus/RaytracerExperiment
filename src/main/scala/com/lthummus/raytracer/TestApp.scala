package com.lthummus.raytracer

import java.io.{File, FileWriter}

import com.lthummus.raytracer.primitive.{Canvas, Color, Point}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.Sphere
import com.lthummus.raytracer.tools.Transformations
import javax.imageio.ImageIO

object TestApp extends App {

  val start = System.currentTimeMillis()

  val rayOrigin = Point(0, 0, -5)
  val wallZ = 10
  val wallSize = 7d

  val canvasPixels = 500
  val pixelSize = wallSize / canvasPixels

  val half = wallSize / 2

  val canvas = Canvas(canvasPixels, canvasPixels)
  val sphereColor = Color.Red

  val sphere = Sphere(Transformations.translation(0, 0, 4))

  for (y <- 0 until canvasPixels) {
    val worldY = half - pixelSize * y
    for (x <- 0 until canvasPixels) {
      val worldX = -half + pixelSize * x
      val pos = Point(worldX, worldY, wallZ)

      val r = Ray(rayOrigin, (pos - rayOrigin).normalized)
      val intersections = sphere.intersections(r)

      if (intersections.nonEmpty) {
        canvas.setPixel(x, y, sphereColor)
      }
    }
  }


  val fw = new FileWriter("test.ppm")
  fw.write(canvas.asPpm)
  fw.close()

  ImageIO.write(canvas.asBufferedImage, "PNG", new File("t.png"))

  val duration = System.currentTimeMillis() - start
  println(s"Duration ${duration}ms")
}
