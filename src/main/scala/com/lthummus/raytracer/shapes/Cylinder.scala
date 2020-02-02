package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Tuple, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.tools.Constants

case class Cylinder(minimum: Double = Double.NegativeInfinity,
                    maximum: Double = Double.PositiveInfinity,
                    closed: Boolean = false,
                    transformation: Matrix = Matrix.Identity4,
                    material: SimpleMaterial = SimpleMaterial.Default) extends Shape {
  override type T = Cylinder

  override private[shapes] def shapeNormalAt(p: Tuple): Tuple = {
    val d = p.x * p.x + p.z * p.z

    if (d < 1 && p.y >= maximum - Constants.Eplison) {
      Vec(0, 1, 0)
    } else if (d < 1 && p.y <= minimum + Constants.Eplison) {
      Vec(0, -1, 0)
    } else {
      Vec(p.x, 0, p.z)
    }
  }

  private def checkCap(r: Ray, t: Double): Boolean = {
    val x = r.origin.x + t * r.direction.x
    val z = r.origin.z + t * r.direction.z

    x * x + z * z <= 1
  }

  private def capIntersections(r: Ray): Seq[Intersection] = {
    if (!closed || r.direction.y.abs < Constants.Eplison) {
      Seq()
    } else {
      val t0 = (minimum - r.origin.y) / r.direction.y
      val potential1 = if (checkCap(r, t0)) Some(Intersection(t0, this)) else None

      val t1 = (maximum - r.origin.y) / r.direction.y
      val potential2 = if (checkCap(r, t1)) Some(Intersection(t1, this)) else None

      Seq(potential1, potential2).flatten
    }
  }

  override private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection] = {
    val a = r.direction.x * r.direction.x + r.direction.z * r.direction.z

    if (a.abs < Constants.Eplison) {
      capIntersections(r)
    } else {
      val b = 2 * r.origin.x * r.direction.x + 2 * r.origin.z * r.direction.z
      val c = r.origin.x * r.origin.x + r.origin.z * r.origin.z - 1

      val d = b * b - 4 * a * c

      if (d < 0) {
        Seq()
      } else {
        val pt0 = (-b - math.sqrt(d)) / (2 * a)
        val pt1 = (-b + math.sqrt(d)) / (2 * a)

        val t0 = math.min(pt0, pt1)
        val t1 = math.max(pt0, pt1)

        val y0 = r.origin.y + t0 * r.direction.y
        val potential1 = if (minimum < y0 && y0 < maximum) {
          Some(Intersection(t0, this))
        } else {
          None
        }

        val y1 = r.origin.y + t1 * r.direction.y
        val potential2 = if (minimum < y1 && y1 < maximum) {
          Some(Intersection(t1, this))
        } else {
          None
        }

        Seq(potential1, potential2).flatten ++ capIntersections(r)
      }
    }
  }
}
