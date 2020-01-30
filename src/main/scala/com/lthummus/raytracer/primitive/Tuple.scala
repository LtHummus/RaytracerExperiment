package com.lthummus.raytracer.primitive

case class Tuple(x: Double, y: Double, z: Double, w: Double) {
  def isPoint: Boolean = w == 1.0d
  def isVector: Boolean = w == 0.0d

  def +(that: Tuple): Tuple = Tuple(this.x + that.x, this.y + that.y, this.z + that.z, this.w + that.w)
  def -(that: Tuple): Tuple = Tuple(this.x - that.x, this.y - that.y, this.z - that.z, this.w - that.w)
  def unary_- : Tuple = Tuple(-x, -y, -z, -w)

  def *(f: Double): Tuple = Tuple(x * f, y * f, z * f, w * f)
  def /(f: Double): Tuple = Tuple(x / f, y / f, z / f, w / f)

  def dot(that: Tuple): Double = this.x * that.x + this.y * that.y + this.z * that.z + this.w * that.w
  def x(that: Tuple): Tuple = Vec(this.y * that.z - this.z * that.y,
                                  this.z * that.x - this.x * that.z,
                                  this.x * that.y - this.y * that.x)

  def magnitude: Double = Math.sqrt(x * x + y * y + z * z + w * w)
  def normalized: Tuple = {
    val m = magnitude
    Tuple(x / m, y / m, z / m, w / m)
  }

  def reflectVector(normal: Tuple): Tuple = this - normal * 2 * (this dot normal)

  def asSeq: Seq[Double] = Seq(x, y, z, w)
}

object Tuple {
  def apply(elements: Array[Double]): Tuple = {
    if (elements.length != 4)
      throw new IllegalArgumentException("Tuples must have exactly 4 elements")

    Tuple(elements(0), elements(1), elements(2), elements(3))
  }
}

object Point {
  val Origin: Tuple = Point(0, 0, 0)

  def apply(x: Double, y: Double, z: Double): Tuple = {
    Tuple(x, y, z, 1.0d)
  }
}

object Vec {
  def apply(x: Double, y: Double, z: Double): Tuple = {
    Tuple(x, y, z, 0.0d)
  }
}
