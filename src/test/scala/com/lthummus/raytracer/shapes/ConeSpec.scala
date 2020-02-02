package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.{SpecConstants, TolerantEquality}
import com.lthummus.raytracer.primitive.{Point, Vec}
import com.lthummus.raytracer.rays.Ray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ConeSpec extends AnyFlatSpec with Matchers with TolerantEquality with SpecConstants {

  "intersections" should "intersect properly" in {
    val c = Cone()

    val testCases = Seq(
      (Point(0, 0, -5), Vec(0, 0, 1), 5.0, 5.0),
      (Point(0, 0, -5), Vec(1, 1, 1), 8.66025, 8.66025),
      (Point(1, 1, -5), Vec(-0.5, -1, 1), 4.55006, 49.44994)
    )

    testCases.foreach{ case(origin, direction, t0, t1) =>
      val r = Ray(origin, direction.normalized)

      val xs = c.shapeIntersectionFrom(r)

      xs must have length 2
      assert(xs(0).t === t0)
      assert(xs(1).t === t1)
    }
  }

  it should "intersect a cone with a ray parallel to the halves" in {
    val c = Cone()
    val r = Ray(Point(0, 0, -1), Vec(0, 1, 1).normalized)

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 1

    assert(xs.head.t === 0.35355)
  }

  it should "handle capped cones" in {
    val c = Cone(-0.5, 0.5, closed = true)

    val testCases = Seq(
      (Point(0, 0, -5), Vec(0, 1, 0), 0),
      (Point(0, 0, -0.25), Vec(0, 1, 1), 2),
      (Point(0, 0, -0.25), Vec(0, 1, 0), 4)
    )

    testCases.foreach{ case (origin, direction, count) =>
      val r = Ray(origin, direction.normalized)

      c.shapeIntersectionFrom(r) must have length count
    }
  }

  "normals" should "be able to be computed" in {
    val c = Cone()

    val testCases = Seq(
      (Point(0, 0, 0), Vec(0, 0, 0)),
      (Point(1, 1, 1), Vec(1, -RootTwo, 1)),
      (Point(-1, -1, 0), Vec(-1, 1, 0))
    )

    testCases.foreach{ case(p, n) =>
      c.shapeNormalAt(p) mustBe n
    }
  }
}
