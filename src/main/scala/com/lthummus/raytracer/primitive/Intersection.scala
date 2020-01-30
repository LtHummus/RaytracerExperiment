package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.Sphere

case class IntersectionInformation(obj: Sphere, t: Double, point: Tuple, eyeVector: Tuple, normalVector: Tuple, inside: Boolean, overPoint: Tuple)

case class Intersection(t: Double, obj: Sphere) {
  def prepareComputation(r: Ray): IntersectionInformation = {
    val point = r.pos(t)
    val eyeVector = -r.direction
    val normalVector = obj.normal(point)
    val overPoint = point + normalVector * Intersection.Epsilon

    if ((normalVector dot eyeVector) < 0) {
      //inside, so flip the normal vector
      IntersectionInformation(obj, t, point, eyeVector, -normalVector, inside = true, overPoint)
    } else {
      IntersectionInformation(obj, t, point, eyeVector, normalVector, inside = false, overPoint)
    }
  }
}

object Intersection {
  private val Epsilon = 1e-4d

  implicit val doubleOrdering = Ordering.Double.TotalOrdering
  implicit def order: Ordering[Intersection] = Ordering.by(_.t)

  implicit class IntersectionList(x: Seq[Intersection]) {
    def hit: Option[Intersection] = x.sorted.find(_.t >= 0)
  }
}