package com.lthummus.raytracer.primitive

import com.typesafe.scalalogging.Logger

case class Color(red: Double, green: Double, blue: Double) {
  def +(that: Color): Color = Color(this.red + that.red, this.green + that.green, this.blue + that.blue)
  def -(that: Color): Color = Color(this.red - that.red, this.green - that.green, this.blue - that.blue)
  def *(factor: Double): Color = Color(this.red * factor, this.green * factor, this.blue * factor)
  def *(that: Color): Color = Color(this.red * that.red, this.green * that.green, this.blue * that.blue)

  private def rgbValues(max: Int): (Int, Int, Int) = {
    val r = (red * max).clamp(0, max - 1).toInt
    val g = (green * max).clamp(0, max - 1).toInt
    val b = (blue * max).clamp(0, max - 1).toInt

    (r, g, b)
  }

  def asPpmString(max: Int): String = {
    val (r, g, b) = rgbValues(max)
    s"$r $g $b"
  }

  def asRgbInt: Int = {
    val max = 255
    val (r, g, b) = rgbValues(max)

    (r << 16) | (g << 8) | b
  }
}

object Color {
  private val Log = Logger("Color")

  def apply(n: Seq[Double]): Color = {
    val patchedNumbers = if (n.length < 3) {
      Log.warn("Color not fully specified. Using 0s to make complete color")
      n ++ Seq.fill(3)(0d)
    } else n
    Color(patchedNumbers(0), patchedNumbers(1), patchedNumbers(2))
  }

  val Black: Color = Color(0, 0, 0)
  val Red: Color = Color(1, 0, 0)
  val Green: Color = Color(0, 1, 0)
  val Blue: Color = Color(0, 0, 1)
  val White: Color = Color(1, 1, 1)
}