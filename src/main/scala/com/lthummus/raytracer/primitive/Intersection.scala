package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.Shape
import com.lthummus.raytracer.tools.Constants

case class IntersectionInformation(obj: Shape, t: Double, point: Tuple, eyeVector: Tuple, normalVector: Tuple, inside: Boolean, overPoint: Tuple, reflectVector: Tuple)

case class Intersection(t: Double, obj: Shape) {
  def prepareComputation(r: Ray): IntersectionInformation = {
    val point = r.pos(t)
    val eyeVector = -r.direction
    val normalVector = obj.normal(point)
    val reflectVector = r.direction.reflectVector(normalVector)

    if ((normalVector dot eyeVector) < 0) {
      //inside, so flip the normal vector
      val overPoint = point + -normalVector * Constants.Eplison
      IntersectionInformation(obj, t, point, eyeVector, -normalVector, inside = true, overPoint, reflectVector)
    } else {
      val overPoint = point + normalVector * Constants.Eplison
      IntersectionInformation(obj, t, point, eyeVector, normalVector, inside = false, overPoint, reflectVector)
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