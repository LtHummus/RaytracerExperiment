package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Tuple, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.tools.Constants

case class Plane(transformation: Matrix = Matrix.Identity4, var material: SimpleMaterial = SimpleMaterial.Default, var parent: Option[Shape] = None) extends Shape {
  override type T = Plane

  override private[shapes] def shapeNormalAt(p: Tuple): Tuple = Vec(0, 1, 0)
  override private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection] = {
    if (r.direction.y.abs < Constants.Eplison) {
      Seq.empty[Intersection]
    } else {
      Seq(Intersection(-r.origin.y / r.direction.y, this))
    }
  }
}
