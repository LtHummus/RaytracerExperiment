package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Matrix, Tuple}
import com.lthummus.raytracer.shapes.Shape

abstract class Pattern {
  type T <: Pattern
  val transform: Matrix

  private[pattern] def colorAt(p: Tuple): Color

  private def pointToPatternPoint(o: Shape, p: Tuple): Tuple = {
    val objectPoint = o.worldToObject(p)
    transform.inverted * objectPoint
  }

  def colorOnObject(p: Tuple, o: Shape): Color = colorAt(pointToPatternPoint(o, p))

}
