package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.Shape
import com.lthummus.raytracer.tools.Constants

case class IntersectionInformation(obj: Shape, t: Double, point: Tuple, eyeVector: Tuple, normalVector: Tuple, inside: Boolean, overPoint: Tuple, underPoint: Tuple, reflectVector: Tuple, n1: Double, n2: Double) {
  def schlick: Double = {
    var cos = eyeVector dot normalVector

    if (n1 > n2) {
      val n = n1 / n2
      val sin2T = n * n * (1.0 - (cos * cos))
      if (sin2T > 1) {
        return 1.0
      }

      cos = Math.sqrt(1.0 - sin2T)
    }

    val r0 = Math.pow((n1 - n2) / (n1 + n2), 2)
    r0 + (1 - r0) * Math.pow(1 - cos, 5)
  }
}

case class Intersection(t: Double, obj: Shape) {

  //TODO: this isn't very "scala" so maybe rewrite it later?
  private def getRefractions(xs: Seq[Intersection]): (Double, Double) = {
    var n1: Double = 1.0d
    var n2: Double = 1.0d

    val containers = scala.collection.mutable.ArrayBuffer.empty[Shape]
    for (i <- xs) {
      if (i == this) {
        if (containers.isEmpty) {
          n1 = 1.0d
        } else {
          n1 = containers.last.material.refractiveIndex
        }
      }

      if (containers.contains(i.obj)) {
        containers -= i.obj
      } else {
        containers += i.obj
      }

      if (i == this) {
        if (containers.isEmpty) {
          n2 = 1.0d
        } else {
          n2 = containers.last.material.refractiveIndex
        }

        return (n1, n2)
      }
    }

    (n1, n2)
  }
  def prepareComputation(r: Ray, xs: Seq[Intersection]): IntersectionInformation = {
    val point = r.pos(t)
    val eyeVector = -r.direction
    val normalVector = obj.normal(point)
    val reflectVector = r.direction.reflectVector(normalVector)

    val (n1, n2) = getRefractions(xs)

    if ((normalVector dot eyeVector) < 0) {
      //inside, so flip the normal vector
      val overPoint = point - normalVector * Constants.Eplison
      val underPoint = point + normalVector * Constants.Eplison
      IntersectionInformation(obj, t, point, eyeVector, -normalVector, inside = true, overPoint, underPoint, reflectVector, n1, n2)
    } else {
      val overPoint = point + normalVector * Constants.Eplison
      val underPoint = point - normalVector * Constants.Eplison
      IntersectionInformation(obj, t, point, eyeVector, normalVector, inside = false, overPoint, underPoint, reflectVector, n1, n2)
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