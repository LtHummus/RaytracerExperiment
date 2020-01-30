package com.lthummus.raytracer

import java.io.{File, FileWriter}

import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
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

  val sphereMaterial = SimpleMaterial.Default.copy(color = Color(1, 0.2, .5))
  val sphere = Sphere(Transformations.translation(0, 0, 4), sphereMaterial)

  val lightPos = Point(-10, 10, -10)
  val lightColor = Color.White
  val light = PointLight(lightPos, lightColor)

  for (y <- 0 until canvasPixels) {
    val worldY = half - pixelSize * y
    for (x <- 0 until canvasPixels) {
      val worldX = -half + pixelSize * x
      val pos = Point(worldX, worldY, wallZ)

      val r = Ray(rayOrigin, (pos - rayOrigin).normalized)
      val intersections = sphere.intersections(r)

      intersections.hit.foreach { hit =>
        val p = r.pos(hit.t)
        val n = hit.obj.normal(p)
        val e = -r.direction

        val c = hit.obj.material.lighting(light, p, e, n)

        canvas.setPixel(x, y, c)
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
