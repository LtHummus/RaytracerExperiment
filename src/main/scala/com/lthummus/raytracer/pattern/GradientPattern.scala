package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Matrix, Tuple}

case class GradientPattern(a: Color, b: Color, transform: Matrix = Matrix.Identity4) extends Pattern {
  override type T = GradientPattern

  override private[pattern] def colorAt(p: Tuple): Color = {
    val d = b - a
    val f = p.x - p.x.floor

    a + d * f
  }
}
