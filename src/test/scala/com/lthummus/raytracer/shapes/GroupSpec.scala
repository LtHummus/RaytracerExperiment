package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.TolerantEquality
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Matrix, Point, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.tools.{RotateY, Scale, Translate}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class GroupSpec extends AnyFlatSpec with Matchers with TolerantEquality {

  "Groups" should "be able to be constructed" in {
    val g = Group()

    g.children mustBe empty
    g.parent mustBe None
    g.transformation mustBe Matrix.Identity4
    g.material mustBe SimpleMaterial.Default
  }

  it should "assign parents on construction" in {
    val s = Sphere.GlassSphere

    s.parent mustBe None

    val g = Group(Seq(s))

    g.children must have length 1
    g.children.head mustBe s

    s.parent mustBe Some(g)
  }

  "intersections" should "work on an empty group" in {
    val g = Group()
    val r = Ray(Point.Origin, Vec(0, 0, 1))

    g.shapeIntersectionFrom(r) must have length 0
  }

  it should "work on a non-empty group" in {
    val s1 = Sphere()
    val s2 = Sphere(Translate(0, 0, -3))
    val s3 = Sphere(Translate(5, 0, 0))

    val g = Group(Seq(s1, s2, s3))

    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))

    val xs = g.shapeIntersectionFrom(r)

    xs must have length 4
    xs(0).obj mustBe s2
    xs(1).obj mustBe s2
    xs(2).obj mustBe s1
    xs(3).obj mustBe s1
  }

  it should "work with transformations" in {
    val s = Sphere(Translate(5, 0, 0))
    val g = Group(Seq(s), Scale(2, 2, 2))

    val r = Ray(Point(10, 0, -10), Vec(0, 0, 1))

    g.intersections(r) must have length 2
  }

  "point conversions" should "translate world to object space" in {
    val s = Sphere(Translate(5, 0, 0))
    val g2 = Group(Seq(s), Scale(2, 2, 2))
    val g1 = Group(Seq(g2), RotateY(math.Pi / 2))

    assert(s.worldToObject(Point(-2, 0, -10)) === Point(0, 0, -1))
  }

  "vector conversions" should "translate normal to world space" in {
    val rootThreeOverThree = math.sqrt(3) / 3

    val s = Sphere(Translate(5, 0, 0))
    val g2 = Group(Seq(s), Scale(1, 2, 3))
    val g1 = Group(Seq(g2), RotateY(math.Pi / 2))

    assert(s.normalToWorld(Vec(rootThreeOverThree, rootThreeOverThree, rootThreeOverThree)) === Vec(0.2857, 0.4286, -0.8571))
  }

  "normals" should "be able to find a normal on a child object" in {
    val s = Sphere(Translate(5, 0, 0))
    val g2 = Group(Seq(s), Scale(1, 2, 3))
    val g1 = Group(Seq(g2), RotateY(math.Pi / 2))

    assert(s.normal(Point(1.7321, 1.1547, -5.5774)) === Vec(0.2857, 0.4286, -0.8571))
  }
}
