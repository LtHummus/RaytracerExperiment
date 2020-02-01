package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.SpecConstants
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.{Plane, Sphere}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class IntersectionSpec extends AnyFlatSpec with Matchers with SpecConstants {

  "Intersection" should "be able to prepare computations" in {
    val s = Sphere()
    val i = Intersection(4, s)
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))

    val info = i.prepareComputation(r)

    info.obj mustBe s
    info.t mustBe 4
    info.point mustBe Point(0, 0, -1)
    info.eyeVector mustBe Vec(0, 0, -1)
    info.normalVector mustBe Vec(0, 0, -1)
    info.inside mustBe false
  }

  it should "be able to tell if intersections happen on the inside" in {
    val s = Sphere()
    val i = Intersection(1, s)
    val r = Ray(Point(0, 0, 0), Vec(0, 0, 1))

    val info = i.prepareComputation(r)

    info.point mustBe Point(0, 0, 1)
    info.eyeVector mustBe Vec(0, 0, -1)
    info.inside mustBe true
    info.normalVector mustBe Vec(0, 0, -1)
  }

  it should "be able to calculate the reflection vector" in {
    val s = Plane()
    val r = Ray(Point(0, 1, -1), Vec(0, -HalfRootTwo, HalfRootTwo))
    val i = Intersection(RootTwo, s)

    i.prepareComputation(r).reflectVector mustBe Vec(0, HalfRootTwo, HalfRootTwo)
  }
}
