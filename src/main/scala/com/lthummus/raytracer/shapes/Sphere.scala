package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Point, Tuple}
import com.lthummus.raytracer.rays.Ray

case class Sphere(transformation: Matrix = Matrix.Identity4, material: SimpleMaterial = SimpleMaterial.Default) extends Shape {
  type T = Sphere

  override private[shapes] def shapeNormalAt(p: Tuple): Tuple = p - Point.Origin

  override private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection] = {
    val sphereToRay = r.origin - Point.Origin

    val a = r.direction dot r.direction
    val b = 2 * (r.direction dot sphereToRay)
    val c = (sphereToRay dot sphereToRay) - 1

    val discriminant = b * b - (4 * a * c)

    if (discriminant < 0) {
      Seq()
    } else {
      val t1 = (-b - Math.sqrt(discriminant)) / (2 * a)
      val t2 = (-b + Math.sqrt(discriminant)) / (2 * a)

      if (t1 < t2) {
        Seq(
          Intersection(t1, this),
          Intersection(t2, this)
        )
      } else {
        Seq(
          Intersection(t2, this),
          Intersection(t1, this)
        )
      }
    }
  }

}

object Sphere {
  val GlassSphere: Sphere = Sphere(Matrix.Identity4, SimpleMaterial.Default.copy(transparency = 1.0, refractiveIndex = 1.5))
}
