package com.lthummus.raytracer

import com.lthummus.raytracer.primitive.Matrix

object TestBench extends App {

  val m = Matrix.Identity4

  println(m)

  println(m.inverted)
}
