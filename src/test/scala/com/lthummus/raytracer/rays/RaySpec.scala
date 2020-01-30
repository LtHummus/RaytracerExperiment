package com.lthummus.raytracer.rays

import com.lthummus.raytracer.primitive.{Point, Vec}
import com.lthummus.raytracer.tools.Transformations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RaySpec extends AnyFlatSpec with Matchers {

  "Ray" should "properly construct" in {
    val p = Point(1, 2, 3)
    val d = Vec(4, 5, 6)

    val r = Ray(p, d)

    r.origin mustBe p
    r.direction mustBe d
  }

  it should "reject vectors as origins" in {
    try {
      Ray(Vec(4, 5, 6), Vec(1, 2, 3))
      fail("Should have thrown an exception")
    } catch {
      case _:IllegalArgumentException => //nop
    }
  }

  it should "reject points as directions" in {
    try {
      Ray(Point(4, 5, 6), Point(1, 2, 3))
      fail("Should have thrown an exception")
    } catch {
      case _:IllegalArgumentException => //nop
    }
  }

  it should "handle calculating distances" in {
    val p = Point(2, 3, 4)
    val d = Vec(1, 0, 0)

    val r = Ray(p, d)

    r.pos(0) mustBe Point(2, 3, 4)
    r.pos(1) mustBe Point(3, 3, 4)
    r.pos(-1) mustBe Point(1, 3, 4)
    r.pos(2.5) mustBe Point(4.5, 3, 4)
  }

  "Ray transformations" should "translate" in {
    val r = Ray(Point(1, 2, 3), Vec(0, 1, 0))
    val t = Transformations.translation(3, 4, 5)

    val r2 = r.transform(t)

    r2.origin mustBe Point(4, 6, 8)
    r2.direction mustBe Vec(0, 1, 0)
  }

  it should "scale" in {
    val r = Ray(Point(1, 2, 3), Vec(0, 1, 0))
    val t = Transformations.scale(2, 3, 4)

    val r2 = r.transform(t)

    r2.origin mustBe Point(2, 6, 12)
    r2.direction mustBe Vec(0, 3, 0)
  }
}
