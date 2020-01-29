package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.primitive.{Intersection, Matrix, Point, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.tools.Transformations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SphereSpec extends AnyFlatSpec with Matchers {

  "Sphere" should "have a default transformation" in {
    val s = Sphere()

    s.transformation mustBe Matrix.Identity4
  }

  it should "apply transformations" in {
    val s = Sphere()
    val t = Transformations.translation(2, 3, 4)

    s.transformation = t
    s.transformation mustBe t
  }

  "Ray/sphere intersections" should "handle the unit sphere intersections properly" in {
    val p = Point(0, 0, -5)
    val d = Vec(0, 0, 1)
    val s = Sphere()

    val r = Ray(p, d)

    val intersections = s.intersections(r)

    intersections must have length 2
    intersections(0) mustBe Intersection(4, s)
    intersections(1) mustBe Intersection(6, s)
  }

  it should "intersections at a tangent" in {
    val p = Point(0, 1, -5)
    val d = Vec(0, 0, 1)
    val s = Sphere()

    val r = Ray(p, d)

    val intersections = s.intersections(r)

    intersections must have length 2
    intersections(0) mustBe Intersection(5, s)
    intersections(1) mustBe Intersection(5, s)
  }

  it should "handle missing a sphere" in {
    val p = Point(0, 2, -5)
    val d = Vec(0, 0, 1)
    val s = Sphere()

    val r = Ray(p, d)

    val intersections = s.intersections(r)

    intersections must have length 0
  }

  it should "handle originating inside the sphere" in {
    val p = Point(0, 0, 0)
    val d = Vec(0, 0, 1)
    val s = Sphere()

    val r = Ray(p, d)

    val intersections = s.intersections(r)

    intersections must have length 2
    intersections(0) mustBe Intersection(-1, s)
    intersections(1) mustBe Intersection(1, s)
  }

  it should "handle sphere behind the ray" in {
    val p = Point(0, 0, 5)
    val d = Vec(0, 0, 1)
    val s = Sphere()

    val r = Ray(p, d)

    val intersections = s.intersections(r)

    intersections must have length 2
    intersections(0) mustBe Intersection(-6, s)
    intersections(1) mustBe Intersection(-4, s)
  }

  it should "handle a scaled sphere" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val t = Transformations.scale(2, 2, 2)
    val s = Sphere(t)

    val intersections = s.intersections(r)

    intersections must have length 2
    intersections(0).t mustBe 3
    intersections(1).t mustBe 7
  }

  it should "handle a translated sphere" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val t = Transformations.translation(5, 0, 0)
    val s = Sphere(t)

    val intersections = s.intersections(r)

    intersections must have length 0
  }
}
