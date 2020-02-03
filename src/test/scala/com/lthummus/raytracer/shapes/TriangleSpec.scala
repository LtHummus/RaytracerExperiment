package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.primitive.{Point, Vec}
import com.lthummus.raytracer.rays.Ray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class TriangleSpec extends AnyFlatSpec with Matchers {

  "Triangles" should "construct and compute their edges and normals" in {
    val p1 = Point(0, 1, 0)
    val p2 = Point(-1, 0, 0)
    val p3 = Point(1, 0, 0)
    val t = Triangle(p1, p2, p3)

    t.p1 mustBe p1
    t.p2 mustBe p2
    t.p3 mustBe p3

    t.e1 mustBe Vec(-1, -1, 0)
    t.e2 mustBe Vec(1, -1, 0)
    t.normal mustBe Vec(0, 0, -1)

    t.shapeNormalAt(Point(0, 0.5, 0)) mustBe Vec(0, 0, -1)
    t.shapeNormalAt(Point(-0.5, 0.75, 0)) mustBe Vec(0, 0, -1)
    t.shapeNormalAt(Point(0.5, 0.25, 0)) mustBe Vec(0, 0, -1)
  }

  "intersections" should "detect rays parallel to the triangle" in {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(0, -1, -2), Vec(0, 1, 0))

    t.shapeIntersectionFrom(r) must have length 0
  }

  it should "have a ray miss the p1-p3 edge" in {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(1, 1, -2), Vec(0, 0, 1))

    t.shapeIntersectionFrom(r) must have length 0
  }

  it should "miss the p1-p2 edge" in {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(-1, 1, -2), Vec(0, 0, 1))

    t.shapeIntersectionFrom(r) must have length 0
  }

  it should "miss the p2-p3 edge" in {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(0, -1, -2), Vec(0, 0, 1))

    t.shapeIntersectionFrom(r) must have length 0
  }

  it should "actually strike the triangle" in {
    val t = Triangle(Point(0, 1, 0), Point(-1, 0, 0), Point(1, 0, 0))
    val r = Ray(Point(0, 0.5, -2), Vec(0, 0, 1))

    val xs = t.shapeIntersectionFrom(r)
    xs must have length 1
    xs.head.t mustBe 2
  }
}
