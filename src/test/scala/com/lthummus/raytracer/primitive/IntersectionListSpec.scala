package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.shapes.Sphere
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class IntersectionListSpec extends AnyFlatSpec with Matchers {

  "IntersectionList" should "be able to identify hits" in {
    val s = Sphere()

    val i1 = Intersection(1, s)
    val i2 = Intersection(2, s)
    val l = Seq(i1, i2)

    l.hit mustBe Some(i1)
  }

  it should "handle some negative intersections" in {
    val s = Sphere()

    val i1 = Intersection(-1, s)
    val i2 = Intersection(2, s)
    val l = Seq(i1, i2)

    l.hit mustBe Some(i2)
  }

  it should "handle when all intersections are negative" in {
    val s = Sphere()

    val i1 = Intersection(-1, s)
    val i2 = Intersection(-2, s)
    val l = Seq(i1, i2)

    l.hit mustBe None
  }

  it should "return first non-negative intersection" in {
    val s = Sphere()

    val i1 = Intersection(5, s)
    val i2 = Intersection(7, s)
    val i3 = Intersection(-3, s)
    val i4 = Intersection(2, s)
    val l = Seq(i1, i2, i3, i4)

    l.hit mustBe Some(i4)
  }
}
