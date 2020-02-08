package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Tuple}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.tools.Constants

case class SmoothTriangle(p1: Tuple,
                     p2: Tuple,
                     p3: Tuple,
                     n1: Tuple,
                     n2: Tuple,
                     n3: Tuple,
                     transformation: Matrix = Matrix.Identity4,
                     var material: SimpleMaterial = SimpleMaterial.Default,
                     var parent: Option[Shape] = None) extends Shape {

  val e1: Tuple = p2 - p1
  val e2: Tuple = p3 - p1
  val normal: Tuple = (e2 x e1).normalized


  override private[shapes] def shapeNormalAt(p: Tuple, info: Option[Intersection] = None): Tuple = {
    info match {
      case None    => throw new IllegalArgumentException("Smooth triangles should have IntersectionInfo")
      case Some(i) =>
        if (i.u.isEmpty || i.v.isEmpty)
          throw new IllegalArgumentException("Smooth triangle tried to compute without u and v")
        val u = i.u.get
        val v = i.v.get

        n2 * u + n3 * v + n1 * (1 - u - v)
    }
  }

  override private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection] = {
    val dirCrossE2 = r.direction x e2
    val det = e1 dot dirCrossE2

    if (det.abs < Constants.Eplison) {
      Seq()
    } else {
      val f = 1.0 / det

      val p1ToOrigin = r.origin - p1
      val u = f * (p1ToOrigin dot dirCrossE2)

      if (u < 0 || u > 1) {
        Seq()
      } else {
        val originCrossE1 = p1ToOrigin x e1
        val v = f * (r.direction dot originCrossE1)
        if (v < 0 || (u + v) > 1) {
          Seq()
        } else {
          val t = f * (e2 dot originCrossE1)
          Seq(Intersection(t, this, Some(u), Some(v)))
        }
      }
    }
  }
}
