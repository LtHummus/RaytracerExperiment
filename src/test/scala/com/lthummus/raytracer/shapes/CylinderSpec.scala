package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.TolerantEquality
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Matrix, Point, Vec}
import com.lthummus.raytracer.rays.Ray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CylinderSpec extends AnyFlatSpec with Matchers with TolerantEquality {

  "cylinders" should "construct properly" in {
    val c = Cylinder()

    c.maximum mustBe Double.PositiveInfinity
    c.minimum mustBe Double.NegativeInfinity
    c.closed mustBe false
    c.transformation mustBe Matrix.Identity4
    c.material mustBe SimpleMaterial.Default
  }

  "intersections" should "validate they miss the cylinder" in {
    val c = Cylinder()

    val r1 = Ray(Point(1, 0, 0), Vec(0, 1, 0).normalized)
    val r2 = Ray(Point(0, 0, 0), Vec(0, 1, 0).normalized)
    val r3 = Ray(Point(0, 0, -5), Vec(1, 1, 1).normalized)

    c.shapeIntersectionFrom(r1) must have length 0
    c.shapeIntersectionFrom(r2) must have length 0
    c.shapeIntersectionFrom(r3) must have length 0
  }

  it should "detect when rays hit the cylinder" in {
    val c = Cylinder()

    val testCases = Seq(
      (Point(1, 0, -5), Vec(0, 0, 1).normalized, 5.0, 5.0),
      (Point(0, 0, -5), Vec(0, 0, 1).normalized, 4.0, 6.0),
      (Point(0.5, 0, -5), Vec(0.1, 1, 1).normalized, 6.80798, 7.08872)
    )

    testCases.foreach{ case(origin, direction, t0, t1) =>
      val r = Ray(origin, direction)
      val xs = c.shapeIntersectionFrom(r)

      xs must have length 2
      assert(xs(0).t === t0)
      assert(xs(1).t === t1)
    }
  }

  it should "work for truncated cylinders" in {
    val c = Cylinder(1, 2)

    val testCases = Seq(
      (Point(0, 1.5, 0), Vec(0.1, 1, 0), 0),
      (Point(0, 3, -5), Vec(0, 0, 1), 0),
      (Point(0, 0, -5), Vec(0, 0, 1), 0),
      (Point(0, 2, -5), Vec(0, 0, 1), 0),
      (Point(0, 1, -5), Vec(0, 0, 1), 0),
      (Point(0, 1.5, -2), Vec(0, 0, 1), 2)
    )

    testCases.foreach{ case(p, direction, count) =>
      val r = Ray(p, direction)

      c.shapeIntersectionFrom(r) must have length count
    }
  }

  it should "work for capped cylinders" in {
    val c = Cylinder(1, 2, closed = true)

    val testCases = Seq(
      (Point(0, 3, 0), Vec(0, -1, 0), 2),
      (Point(0, 3, -2), Vec(0, -1, 2), 2),
      (Point(0, 4, -2), Vec(0, -1, 1), 2),
      (Point(0, 0, -2), Vec(0, 1, 2), 2),
      (Point(0, -1, -2), Vec(0, 1, 1), 2)
    )

    testCases.foreach{ case(p, direction, count) =>
      val r = Ray(p, direction)

      c.shapeIntersectionFrom(r) must have length count
    }
  }

  "normals" should "work" in {
    val c = Cylinder()

    val testCases = Seq(
      (Point(1, 0, 0), Vec(1, 0, 0)),
      (Point(0, 5, -1), Vec(0, 0, -1)),
      (Point(0, -2, 1), Vec(0, 0, 1)),
      (Point(-1, 1, 0), Vec(-1, 0, 0))
    )

    testCases.foreach { case(p, n) =>
      c.shapeNormalAt(p) mustBe n
    }
  }

  it should "work for caps cylinders" in {
    val c = Cylinder(1, 2, closed = true)

    val testCases = Seq(
      (Point(0, 1, 0), Vec(0, -1, 0)),
      (Point(0.5, 1, 0), Vec(0, -1, 0)),
      (Point(0, 1, 0.5), Vec(0, -1, 0)),
      (Point(0, 2, 0), Vec(0, 1, 0)),
      (Point(0.5, 2, 0), Vec(0, 1, 0)),
      (Point(0, 2, 0.5), Vec(0, 1, 0))
    )

    testCases.foreach { case(p, n) =>
      c.shapeNormalAt(p) mustBe n
    }
  }
}
