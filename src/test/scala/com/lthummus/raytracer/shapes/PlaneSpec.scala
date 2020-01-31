package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.primitive.{Point, Vec}
import com.lthummus.raytracer.rays.Ray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class PlaneSpec extends AnyFlatSpec with Matchers {

  "the norma" should "be constant everywhere" in {
    val p = Plane()

    p.shapeNormalAt(Point(0, 0, 0)) mustBe Vec(0, 1, 0)
    p.shapeNormalAt(Point(10, 0, -10)) mustBe Vec(0, 1, 0)
    p.shapeNormalAt(Point(-5, 0, 150)) mustBe Vec(0, 1, 0)
  }

  "intersections" should "work with a ray parallel to the plane" in {
    val p = Plane()
    val r = Ray(Point(0, 10, 0), Vec(0, 0, 1))

    p.shapeIntersectionFrom(r) must have length 0
  }

  it should "work with a coplanar ray" in {
    val p = Plane()
    val r = Ray(Point(0, 0, 0), Vec(0, 0, 1))

    p.shapeIntersectionFrom(r) must have length 0
  }

  it should "work with rays from above" in {
    val p = Plane()
    val r = Ray(Point(0, 1, 0), Vec(0, -1, 0))

    val intersections = p.shapeIntersectionFrom(r)

    intersections must have length 1
    intersections.head.t mustBe 1
    intersections.head.obj mustBe p
  }

  it should "work with rays from below" in {
    val p = Plane()
    val r = Ray(Point(0, -1, 0), Vec(0, 1, 0))

    val intersections = p.shapeIntersectionFrom(r)

    intersections must have length 1
    intersections.head.t mustBe 1
    intersections.head.obj mustBe p
  }
}
