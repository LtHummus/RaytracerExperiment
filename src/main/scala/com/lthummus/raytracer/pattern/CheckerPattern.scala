package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Matrix, Tuple}
import com.lthummus.raytracer.tools.Constants

case class CheckerPattern(a: Color, b: Color, transform: Matrix = Matrix.Identity4) extends Pattern {
  override type T = CheckerPattern

  override private[pattern] def colorAt(p: Tuple): Color = {
    val pointSum = (p.x + Constants.Eplison).floor + (p.y + Constants.Eplison).floor + (p.z + Constants.Eplison).floor
    if ((pointSum % 2).abs < Constants.Eplison) a else b
  }
}
