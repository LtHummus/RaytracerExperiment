package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.shapes.Sphere

case class Intersection(t: Double, obj: Sphere)

object Intersection {
  implicit val doubleOrdering = Ordering.Double.TotalOrdering

  implicit class IntersectionList(x: Seq[Intersection]) {
    def hit: Option[Intersection] = x.sortBy(_.t).find(_.t >= 0)
  }
}