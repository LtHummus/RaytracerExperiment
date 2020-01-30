package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Point, Tuple, Vec}
import com.lthummus.raytracer.rays.Ray

case class Sphere(var transformation: Matrix = Matrix.Identity4, var material: SimpleMaterial = SimpleMaterial.Default) {

  def normal(p: Tuple): Tuple = {
    require(p.isPoint)

    val objectPoint = transformation.inverted * p
    val objectNormal = objectPoint - Point(0, 0, 0)
    val worldNormal = transformation.inverted.transpose * objectNormal
    worldNormal.copy(w = 0).normalized
  }

  def reflect(p: Tuple): Tuple = reflectVector(p, normal(p))

  def intersections(r: Ray): Seq[Intersection] = {
    val r2 = r.transform(transformation.inverted)
    val sphereToRay = r2.origin - Point(0, 0, 0)

    val a = r2.direction dot r2.direction
    val b = 2 * (r2.direction dot sphereToRay)
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
