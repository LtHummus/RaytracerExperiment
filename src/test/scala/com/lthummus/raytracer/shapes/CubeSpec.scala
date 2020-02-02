package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.primitive.{Point, Vec}
import com.lthummus.raytracer.rays.Ray
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CubeSpec extends AnyFlatSpec with Matchers {

  "cube intersections" should "work from the +x direction" in {
    val c = Cube()
    val r = Ray(Point(5, 0.5, 0), Vec(-1, 0, 0))

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 2
    xs(0).t mustBe 4
    xs(1).t mustBe 6
  }

  it should "work from the -x direction" in {
    val c = Cube()
    val r = Ray(Point(-5, 0.5, 0), Vec(1, 0, 0))

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 2
    xs(0).t mustBe 4
    xs(1).t mustBe 6
  }

  it should "work from the +y direction" in {
    val c = Cube()
    val r = Ray(Point(0.5, 5, 0), Vec(0, -1, 0))

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 2
    xs(0).t mustBe 4
    xs(1).t mustBe 6
  }

  it should "work from the -y direction" in {
    val c = Cube()
    val r = Ray(Point(0.5, -5, 0), Vec(0, 1, 0))

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 2
    xs(0).t mustBe 4
    xs(1).t mustBe 6
  }

  it should "work from the +z direction" in {
    val c = Cube()
    val r = Ray(Point(0.5, 0, 5), Vec(0, 0, -1))

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 2
    xs(0).t mustBe 4
    xs(1).t mustBe 6
  }

  it should "work from the -z direction" in {
    val c = Cube()
    val r = Ray(Point(0.5, 0, -5), Vec(0, 0, 1))

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 2
    xs(0).t mustBe 4
    xs(1).t mustBe 6
  }

  it should "work from inside the cube" in {
    val c = Cube()
    val r = Ray(Point(0, 0.5, 0), Vec(0, 0, 1))

    val xs = c.shapeIntersectionFrom(r)

    xs must have length 2
    xs(0).t mustBe -1
    xs(1).t mustBe 1
  }

  it should "handle several rays that all miss the cube" in {
    val testCases = Seq(
      (Point(-2, 0, 0), Vec(0.2673, 0.5345, 0.8108)),
      (Point(0, -2, 0), Vec(0.8108, 0.2673, 0.5345)),
      (Point(0, 0, -2), Vec(0.5345, 0.8108, 0.2673)),
      (Point(2, 0, 2), Vec(0, 0, -1)),
      (Point(0, 2, 2), Vec(0, -1, 0)),
      (Point(2, 2, 0), Vec(-1, 0, 0))
    )

    val c = Cube()

    testCases.foreach { case (origin, direction) =>
      val r = Ray(origin, direction)

      c.shapeIntersectionFrom(r) must have length 0
    }
  }

  "cube normals" should "calculate normals in a consistent way" in {
    val testCases = Seq(
      (Point(1, 0.5, -0.8), Vec(1, 0, 0)),
      (Point(-1, -0.2, 0.9), Vec(-1, 0, 0)),
      (Point(-0.4, 1, -0.1), Vec(0, 1, 0)),
      (Point(0.3, -1, -0.7), Vec(0, -1, 0)),
      (Point(-0.6, 0.3, 1), Vec(0, 0, 1)),
      (Point(0.4, 0.4, -1), Vec(0, 0, -1)),
      (Point(1, 1, 1), Vec(1, 0, 0)),
      (Point(-1, -1, -1), Vec(-1, 0, 0))
    )

    val c = Cube()

    testCases.foreach { case (point, expectedNormal) =>
      c.shapeNormalAt(point) mustBe expectedNormal
    }
  }
}
