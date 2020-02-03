package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.TolerantEquality
import com.lthummus.raytracer.primitive.{Intersection, Point, Vec}
import com.lthummus.raytracer.rays.Ray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SmoothTriangleSpec extends AnyFlatSpec with Matchers with TolerantEquality {

  "SmoothTriangle" should "construct properly" in {
    val t = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0), Vec(0, 1, 0), Vec(-1, 0, 0), Vec(1, 0, 0))

    t.p1 mustBe Point(0, 1, 0)
    t.p2 mustBe Point(-1, 0, 0)
    t.p3 mustBe Point(1, 0, 0)

    t.n1 mustBe Vec(0, 1, 0)
    t.n2 mustBe Vec(-1, 0, 0)
    t.n3 mustBe Vec(1, 0, 0)
  }

  it should "intersect with u and v" in {
    val t = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0), Vec(0, 1, 0), Vec(-1, 0, 0), Vec(1, 0, 0))
    val r = Ray(Point(-0.2, 0.3, -2), Vec(0, 0, 1))
    val xs = t.shapeIntersectionFrom(r)

    xs must have length 1
    assert(xs.head.u === Some(0.45))
    assert(xs.head.v === Some(0.25))
  }

  it should "interpolate normals" in {
    val t = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0), Vec(0, 1, 0), Vec(-1, 0, 0), Vec(1, 0, 0))
    val i = Intersection(1, t, Some(0.45), Some(0.25))

    assert(t.normal(Point(0, 0, 0), Some(i)) === Vec(-0.5547, 0.83205, 0))
  }

  it should "prepare the normal on a smooth triangle" in {
    val t = SmoothTriangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0), Vec(0, 1, 0), Vec(-1, 0, 0), Vec(1, 0, 0))
    val i = Intersection(1, t, Some(0.45), Some(0.25))
    val r = Ray(Point(-0.2, 0.3, -2), Vec(0, 0, 1))

    val xs = Seq(i)
    val info = i.prepareComputation(r, xs)

    assert(info.normalVector === Vec(-0.5547, 0.83205, 0))
  }
}
