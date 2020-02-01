package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Matrix, Tuple}
import com.lthummus.raytracer.tools.Constants

case class CheckerPattern(a: Color, b: Color, transform: Matrix = Matrix.Identity4) extends Pattern {
  override type T = CheckerPattern

  override private[pattern] def colorAt(p: Tuple): Color = {
    val pointSum = p.x.floor + p.y.floor + p.z.floor
    if ((pointSum % 2) == 0) a else b
  }
}
