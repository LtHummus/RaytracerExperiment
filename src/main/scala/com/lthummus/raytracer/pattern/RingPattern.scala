package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Matrix, Tuple}
import com.lthummus.raytracer.tools.Constants

case class RingPattern(a: Color, b: Color, transform: Matrix = Matrix.Identity4) extends Pattern {
  override type T = RingPattern

  override private[pattern] def colorAt(p: Tuple): Color = if ((Math.sqrt(p.x * p.x + p.z * p.z) + Constants.Eplison).floor % 2 == 0) a else b

}