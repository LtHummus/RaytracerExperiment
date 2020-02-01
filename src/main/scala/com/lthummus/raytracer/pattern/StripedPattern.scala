package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Matrix, Tuple}

case class StripedPattern(a: Color, b: Color, transform: Matrix = Matrix.Identity4) extends Pattern {
  type T = StripedPattern

  private[pattern] def colorAt(p: Tuple): Color = if (p.x.floor % 2 == 0) a else b

}

