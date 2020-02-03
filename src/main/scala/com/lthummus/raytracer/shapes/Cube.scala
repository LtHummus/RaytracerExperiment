package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Tuple, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.tools.Constants

case class Cube(transformation: Matrix = Matrix.Identity4, var material: SimpleMaterial = SimpleMaterial.Default, var parent: Option[Shape] = None) extends Shape {
  override type T = Cube

  override private[shapes] def shapeNormalAt(p: Tuple, info: Option[Intersection] = None): Tuple = {
    val maxComponent = math.max(math.max(p.x.abs, p.y.abs), p.z.abs)

    if (maxComponent == p.x.abs) {
      Vec(p.x, 0, 0)
    } else if (maxComponent == p.y.abs) {
      Vec(0, p.y, 0)
    } else {
      Vec(0, 0, p.z)
    }
  }

  private def checkAxis(origin: Double, direction: Double): (Double, Double) = {
    val tMinNumerator = -1 - origin
    val tMaxNumerator = 1 - origin

    //TODO: potentially get away without explicitly checking for very small direction
    val (tMin, tMax) = if (direction.abs >= Constants.Eplison) {
      (tMinNumerator / direction, tMaxNumerator / direction)
    } else {
      (tMinNumerator * Double.PositiveInfinity, tMaxNumerator * Double.PositiveInfinity)
    }

    if (tMin > tMax) {
      (tMax, tMin)
    } else {
      (tMin, tMax)
    }
  }

  override private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection] = {
    val (xtMin, xtMax) = checkAxis(r.origin.x, r.direction.x)
    val (ytMin, ytMax) = checkAxis(r.origin.y, r.direction.y)
    val (ztMin, ztMax) = checkAxis(r.origin.z, r.direction.z)

    val tMin = math.max(math.max(xtMin, ytMin), ztMin)
    val tMax = math.min(math.min(xtMax, ytMax), ztMax)

    if (tMax < tMin) {
      Seq()
    } else {
      Seq(
        Intersection(tMin, this),
        Intersection(tMax, this)
      )
    }
  }
}
