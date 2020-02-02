package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.{SpecConstants, TolerantEquality}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.{Plane, Sphere}
import com.lthummus.raytracer.tools.{Scale, Translate}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class IntersectionSpec extends AnyFlatSpec with Matchers with SpecConstants with TolerantEquality {

  "Intersection" should "be able to prepare computations" in {
    val s = Sphere()
    val i = Intersection(4, s)
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))

    val info = i.prepareComputation(r, Seq(i))

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

    val info = i.prepareComputation(r, Seq(i))

    info.point mustBe Point(0, 0, 1)
    info.eyeVector mustBe Vec(0, 0, -1)
    info.inside mustBe true
    info.normalVector mustBe Vec(0, 0, -1)
  }

  it should "be able to calculate the reflection vector" in {
    val s = Plane()
    val r = Ray(Point(0, 1, -1), Vec(0, -HalfRootTwo, HalfRootTwo))
    val i = Intersection(RootTwo, s)

    i.prepareComputation(r, Seq(i)).reflectVector mustBe Vec(0, HalfRootTwo, HalfRootTwo)
  }

  it should "be able to figure out the underpoint" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = Sphere.GlassSphere.copy(Translate(0, 0, 1))

    val i = Intersection(5, s)
    val xs = Seq(i)

    val info = i.prepareComputation(r, xs)

    info.underPoint.z mustBe > (1e-4d / 2)
    info.point.z mustBe < (1e-4d / 2)
  }

  it should "find n1 and n2 at intersections" in {
    val baseMaterial = Sphere.GlassSphere.material
    val a = Sphere.GlassSphere.copy(transformation = Scale(2, 2, 2), baseMaterial.copy(refractiveIndex = 1.5))
    val b = Sphere.GlassSphere.copy(transformation = Translate(0, 0, -0.25), baseMaterial.copy(refractiveIndex = 2.0))
    val c = Sphere.GlassSphere.copy(transformation = Translate(0, 0, -0.25), baseMaterial.copy(refractiveIndex = 2.5))

    val r = Ray(Point(0, 0, -4), Vec(0, 0, 1))
    val i = Seq(
      Intersection(2, a),
      Intersection(2.75, b),
      Intersection(3.25, c),
      Intersection(4.75, b),
      Intersection(5.25, c),
      Intersection(7, a),
    )

    val n1s = Seq(1.0, 1.5, 2.0, 2.5, 2.5, 1.5)
    val n2s = Seq(1.5, 2.0, 2.5, 2.5, 1.5, 1.0)

    for (x <- i.indices) {
      val res = i(x).prepareComputation(r, i)

      assert(res.n1 === n1s(x), s"on idx $x")
      assert(res.n2 === n2s(x), s"on idx $x")

    }
  }

  "Schlick" should "work under total internal reflection" in {
    val s = Sphere.GlassSphere
    val r = Ray(Point(0, 0, HalfRootTwo), Vec(0, 1, 0))

    val xs = Seq(
      Intersection(-HalfRootTwo, s),
      Intersection(HalfRootTwo, s)
    )

    val info = xs(1).prepareComputation(r, xs)

    info.schlick mustBe 1.0d
  }

  it should "approximate a perpendicular viewing angle" in {
    val s = Sphere.GlassSphere
    val r = Ray(Point(0, 0, 0), Vec(0, 1, 0))

    val xs = Seq(
      Intersection(-1, s),
      Intersection(1, s)
    )

    val info = xs(1).prepareComputation(r, xs)

    assert(info.schlick === 0.04d)
  }

  it should "approximate with small angle + n2 > n1" in {
    val s = Sphere.GlassSphere
    val r = Ray(Point(0, 0.99, -2), Vec(0, 0, 1))
    val xs = Seq(Intersection(1.8589, s))

    val info = xs.head.prepareComputation(r, xs)

    assert(info.schlick === 0.48873)
  }
}
