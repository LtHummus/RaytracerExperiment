package com.lthummus.raytracer.transformation

import com.lthummus.raytracer.{SpecConstants, TolerantEquality}
import com.lthummus.raytracer.primitive.{Matrix, Point, Vec}
import com.lthummus.raytracer.tools.Transformations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class TransformationsSpec extends AnyFlatSpec with Matchers with TolerantEquality with SpecConstants {

  "Translation" should "produce a proper translation matrix" in {
    val t = Transformations.translation(5, -3, 2)
    val p = Point(-3, 4, 5)

    t * p mustBe Point(2, 1, 7)
  }

  it should "work when the translation matrix is inverted" in {
    val t = Transformations.translation(5, -3, 2)
    val p = Point(-3, 4, 5)

    t.inverted * p mustBe Point(-8, 7, 3)
  }

  it should "not affect vectors" in {
    val t = Transformations.translation(5, -3, 2)
    val v = Vec(-3, 4, 5)

    t * v mustBe v
  }

  "Scale" should "scale a point" in {
    val t = Transformations.scale(2, 3, 4)
    val p = Point(-4, 6, 8)

    t * p mustBe Point(-8, 18, 32)
  }

  it should "scale a vector" in {
    val t = Transformations.scale(2, 3, 4)
    val v = Vec(-4, 6, 8)

    t * v mustBe Vec(-8, 18, 32)
  }

  it should "inverse scale a vector when inverted" in {
    val t = Transformations.scale(2, 3, 4)
    val v = Vec(-4, 6, 8)

    t.inverted * v mustBe Vec(-2, 2, 2)
  }

  it should "handle reflection as expected" in {
    val t = Transformations.scale(-1, 1, 1)
    val p = Point(2, 3, 4)

    t * p mustBe Point(-2, 3, 4)
  }

  "rotation around X" should "handle a couple rotations" in {
    val p = Point(0, 1, 0)

    val halfQuarter = Transformations.rotateX(Math.PI / 4)
    val fullQuarter = Transformations.rotateX(Math.PI / 2)

    assert(halfQuarter * p === Point(0, HalfRootTwo, HalfRootTwo))
    assert(fullQuarter * p === Point(0, 0, 1))

  }

  it should "handle inverse rotations" in {
    val p = Point(0, 1, 0)
    val halfQuarter = Transformations.rotateX(Math.PI / 4)

    assert(halfQuarter.inverted * p === Point(0, HalfRootTwo, -HalfRootTwo))
  }

  "rotation around Y" should "handle a couple rotations" in {
    val p = Point(0, 0, 1)

    val halfQuarter = Transformations.rotateY(Math.PI / 4)
    val fullQuarter = Transformations.rotateY(Math.PI / 2)

    assert(halfQuarter * p === Point(HalfRootTwo, 0, HalfRootTwo))
    assert(fullQuarter * p === Point(1, 0, 0))
  }

  "rotation around Z" should "handle a couple rotations" in {
    val p = Point(0, 1, 0)

    val halfQuarter = Transformations.rotateZ(Math.PI / 4)
    val fullQuarter = Transformations.rotateZ(Math.PI / 2)

    assert(halfQuarter * p === Point(-HalfRootTwo, HalfRootTwo, 0))
    assert(fullQuarter * p === Point(-1, 0, 0))
  }

  "shearing" should "move x in proportion to y" in {
    val t = Transformations.sheer(1, 0, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    t * p mustBe Point(5, 3, 4)
  }

  it should "move x in proportion to z" in {
    val t = Transformations.sheer(0, 1, 0, 0, 0, 0)
    val p = Point(2, 3, 4)

    t * p mustBe Point(6, 3, 4)
  }

  it should "move y in proportion to x" in {
    val t = Transformations.sheer(0, 0, 1, 0, 0, 0)
    val p = Point(2, 3, 4)

    t * p mustBe Point(2, 5, 4)
  }

  it should "move y in proportion to z" in {
    val t = Transformations.sheer(0, 0, 0, 1, 0, 0)
    val p = Point(2, 3, 4)

    t * p mustBe Point(2, 7, 4)
  }

  it should "move z in proportion to x" in {
    val t = Transformations.sheer(0, 0, 0, 0, 1, 0)
    val p = Point(2, 3, 4)

    t * p mustBe Point(2, 3, 6)
  }

  it should "move z in proportion to y" in {
    val t = Transformations.sheer(0, 0, 0, 0, 0, 1)
    val p = Point(2, 3, 4)

    t * p mustBe Point(2, 3, 7)
  }

  "chaining transformations" should "work" in {
    val p = Point(1, 0, 1)

    val a = Transformations.rotateX(Math.PI / 2)
    val b = Transformations.scale(5, 5, 5)
    val c = Transformations.translation(10, 5, 7)

    val p2 = a * p
    assert(p2 === Point(1, -1, 0))

    val p3 = b * p2
    assert(p3 === Point(5, -5, 0))

    val p4 = c * p3
    assert(p4 === Point(15, 0, 7))
  }

  it should "also follow associativity rule" in {
    val p = Point(1, 0, 1)

    val a = Transformations.rotateX(Math.PI / 2)
    val b = Transformations.scale(5, 5, 5)
    val c = Transformations.translation(10, 5, 7)

    val t = c * b * a
    t * p mustBe Point(15, 0, 7)
  }

  "view transformation" should "figure out the matrix for default orientation" in {
    val from = Point(0, 0, 0)
    val to = Point(0, 0, -1)
    val up = Vec(0, 1, 0)

    Transformations.viewTransform(from, to, up) mustBe Matrix.Identity4
  }

  it should "work when looking at the positive z direction" in {
    val from = Point(0, 0, 0)
    val to = Point(0, 0, 1)
    val up = Vec(0, 1, 0)

    Transformations.viewTransform(from, to, up) mustBe Transformations.scale(-1, 1, -1) //up-down same, left-right & front-back flipped
  }

  it should "move the world" in {
    val from = Point(0, 0, 8)
    val to = Point(0, 0, 0)
    val up = Vec(0, 1, 0)

    Transformations.viewTransform(from, to, up) mustBe Transformations.translation(0, 0, -8)
  }

  it should "work on arbitrary transformations" in {
    val from = Point(1, 3, 2)
    val to = Point(4, -2, 8)
    val up = Vec(1, 1, 0)

    val t = Transformations.viewTransform(from, to, up)

    val correct = Matrix(
      -0.50709,  0.50709,  0.67612, -2.36643,
       0.76772,  0.60609,  0.12122, -2.82843,
      -0.35857,  0.59761, -0.71714,  0.00000,
       0.00000,  0.00000,  0.00000,  1.00000
    )

    assert(t === correct)
  }

}
