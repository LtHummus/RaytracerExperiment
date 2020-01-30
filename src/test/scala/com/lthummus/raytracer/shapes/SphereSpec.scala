package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.TolerantEquality
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Point, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.tools.Transformations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SphereSpec extends AnyFlatSpec with Matchers with TolerantEquality {

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

  it should "have a default material" in {
    val s = Sphere()

    s.material mustBe SimpleMaterial.Default
  }

  it should "allow new materials" in {
    val m = SimpleMaterial.Default.copy(ambient = 1.0)
    val s = Sphere(Matrix.Identity4, m)

    s.material.ambient mustBe 1.0

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

  "normal vectors" should "calculate properly at x-axis" in {
    val s = Sphere()
    val n = s.normal(Point(1, 0, 0))

    n mustBe Vec(1, 0, 0)
  }

  it should "calculate properly at y-axis" in {
    val s = Sphere()
    val n = s.normal(Point(0, 1, 0))

    n mustBe Vec(0, 1, 0)
  }

  it should "calculate properly at z-axis" in {
    val s = Sphere()
    val n = s.normal(Point(0, 0, 1))

    n mustBe Vec(0, 0, 1)
  }

  it should "calculate properly at some nonaxial point" in {
    val rootThreeOverThree = Math.sqrt(3) / 3
    val s = Sphere()
    val n = s.normal(Point(rootThreeOverThree, rootThreeOverThree, rootThreeOverThree))

    n mustBe Vec(rootThreeOverThree, rootThreeOverThree, rootThreeOverThree)
  }

  it should "return a normalized vector" in {
    val rootThreeOverThree = Math.sqrt(3) / 3
    val s = Sphere()
    val n = s.normal(Point(rootThreeOverThree, rootThreeOverThree, rootThreeOverThree))

    n.normalized mustBe n
  }

  it should "work on a transformed sphere" in {
    val s = Sphere(Transformations.translation(0, 1, 0))
    val n = s.normal(Point(0, 1.70711, -0.70711))

    assert(n === Vec(0, 0.707106, -0.707106))
  }

  it should "work on multiple transformations" in {
    val rootTwoOverTwo = Math.sqrt(2) / 2
    val t = Transformations.scale(1, 0.5, 1) * Transformations.rotateZ(Math.PI / 5)
    val s = Sphere(t)

    val n = s.normal(Point(0, rootTwoOverTwo, -rootTwoOverTwo))

    assert(n === Vec(0, 0.9701425, -0.2425356))
    n.normalized mustBe n
  }

  "reflection vectors" should "calculate at a 45 degree angle" in {
    val v = Vec(1, -1, 0)
    val n = Vec(0, 1, 0)

    reflectVector(v, n) mustBe Vec(1, 1, 0)
  }

  it should "calculate at an arbitrary angle" in {
    val rootTwoOverTwo = Math.sqrt(2) / 2

    val v = Vec(0, -1, 0)
    val n = Vec(rootTwoOverTwo, rootTwoOverTwo, 0)

    val r = reflectVector(v, n)

    assert(r === Vec(1, 0, 0))

  }


}
