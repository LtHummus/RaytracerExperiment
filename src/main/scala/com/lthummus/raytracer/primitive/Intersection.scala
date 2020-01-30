package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.Sphere

case class IntersectionInformation(obj: Sphere, t: Double, point: Tuple, eyeVector: Tuple, normalVector: Tuple, inside: Boolean)

case class Intersection(t: Double, obj: Sphere) {
  def prepareComputation(r: Ray): IntersectionInformation = {
    val point = r.pos(t)
    val eyeVector = -r.direction
    val normalVector = obj.normal(point)

    if ((normalVector dot eyeVector) < 0) {
      //inside, so flip the normal vector
      IntersectionInformation(obj, t, point, eyeVector, -normalVector, inside = true)
    } else {
      IntersectionInformation(obj, t, point, eyeVector, normalVector, inside = false)
    }
  }
}

object Intersection {
  implicit val doubleOrdering = Ordering.Double.TotalOrdering
  implicit def order: Ordering[Intersection] = Ordering.by(_.t)

  implicit class IntersectionList(x: Seq[Intersection]) {
    def hit: Option[Intersection] = x.sorted.find(_.t >= 0)
  }
}