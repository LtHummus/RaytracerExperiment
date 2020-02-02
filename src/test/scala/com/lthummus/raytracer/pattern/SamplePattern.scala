package com.lthummus.raytracer.pattern
import com.lthummus.raytracer.primitive.{Color, Matrix, Tuple}

case class SamplePattern() extends Pattern {
  override type T = SamplePattern
  override val transform: Matrix = Matrix.Identity4

  override private[pattern] def colorAt(p: Tuple): Color = Color(p.x, p.y, p.z)
}
