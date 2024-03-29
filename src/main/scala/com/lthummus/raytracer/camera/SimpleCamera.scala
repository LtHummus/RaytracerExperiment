package com.lthummus.raytracer.camera

import com.lthummus.raytracer.primitive.{Canvas, Matrix, Point, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.world.World

import scala.collection.parallel.CollectionConverters._

case class SimpleCamera(horizSize: Int, vertSize: Int, fov: Double, transform: Matrix = Matrix.Identity4) {
  val halfView: Double = Math.tan(fov / 2)
  val aspect: Double = horizSize.toDouble / vertSize

  val (halfWidth: Double, halfHeight: Double) = {
    if (aspect >= 1) {
      (halfView, halfView / aspect)
    } else {
      (halfView * aspect, halfView)
    }
  }

  lazy val pixelSize: Double = halfWidth * 2 / horizSize


  def rayForPixel(x: Double, y: Double): Ray = {
    //offset from edge of canvas to center
    val xOffset = (x + 0.5) * pixelSize
    val yOffset = (y + 0.5) * pixelSize

    //untransformed coords of pixel in world space
    val worldX = halfWidth - xOffset
    val worldY = halfHeight - yOffset

    //transform everything and figure out the ray
    val pixel = transform.inverted * Point(worldX, worldY, -1)
    val origin = transform.inverted * Point.Origin
    val direction = (pixel - origin).normalized

    Ray(origin, direction)
  }

  private def handlePixel(world: World, canvas: Canvas)(coords: (Int, Int)): Unit = {
    val (x, y) = coords
    val r = rayForPixel(x, y)
    val c = world.colorAt(r)

    canvas.setPixel(x, y, c)
  }

  def render(world: World, parallel: Boolean = true): Canvas = {
    val canvas = Canvas(horizSize, vertSize)

    val allPoints = for {
      y <- 0 until vertSize
      x <- 0 until horizSize
    } yield (x, y)

    val pixelHandler: ((Int, Int)) => Unit = handlePixel(world, canvas)

    if (parallel) {
      allPoints.par.foreach(pixelHandler)
    } else {
      allPoints.foreach(pixelHandler)
    }



    canvas
  }
}
