package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.{TolerantEquality, primitive}
import com.lthummus.raytracer.primitive.{Point, Tuple, Vec}
import org.scalactic.TolerantNumerics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class TupleSpec extends AnyFlatSpec with Matchers with TolerantEquality {

  "Tuples" should "identify as a point or vector properly" in {
    val point = Point(1, 2, 3)
    val vector = Vec(4, 5, 6)

    point.x mustBe 1
    point.y mustBe 2
    point.z mustBe 3

    vector.x mustBe 4
    vector.y mustBe 5
    vector.z mustBe 6

    point.isPoint mustBe true
    point.isVector mustBe false

    vector.isPoint mustBe false
    vector.isVector mustBe true
  }

  it should "properly add" in {
    val x = Tuple(3, -2, 5, 1)
    val y = Tuple(-2, 3, 1, 0)

    val s = x + y

    s.x mustBe 1
    s.y mustBe 1
    s.z mustBe 6
    s.w mustBe 1
  }

  it should "properly subtract" in {
    val x = Tuple(3, -2, 5, 1)
    val y = Tuple(-2, 3, 1, 0)

    val s = x - y

    s.x mustBe 5
    s.y mustBe -5
    s.z mustBe 4
    s.w mustBe 1

    val x2 = Point(1, 2, 5)
    val y2 = Point(-1, -1, -1)

    val s2 = x2 - y2

    s2.x mustBe 2
    s2.y mustBe 3
    s2.z mustBe 6
    s2.isVector mustBe true
  }

  it should "properly negate" in {
    val x = Tuple(3, 6, 9, 12)
    val s = -x

    s.x mustBe -3
    s.y mustBe -6
    s.z mustBe -9
    s.w mustBe -12
  }

  it should "properly scalar multiply" in {
    val x = Tuple(2, 4, 6, 8)
    val s = x * 2

    s.x mustBe 4
    s.y mustBe 8
    s.z mustBe 12
    s.w mustBe 16

    val s2 = x * 0.5d
    s2.x mustBe 1
    s2.y mustBe 2
    s2.z mustBe 3
    s2.w mustBe 4
  }

  it should "properly scalar divide" in {
    val x = Tuple(2, 4, 6, 8)
    val s = x / 2

    s.x mustBe 1
    s.y mustBe 2
    s.z mustBe 3
    s.w mustBe 4

    val s2 = x / 0.5d
    s2.x mustBe 4
    s2.y mustBe 8
    s2.z mustBe 12
    s2.w mustBe 16
  }

  it should "properly compute a magnitude" in {
    Tuple(1, 0, 0, 0).magnitude mustBe 1
    Tuple(0, 1, 0, 0).magnitude mustBe 1
    Tuple(0, 0, 1, 0).magnitude mustBe 1
    Tuple(0, 0, 0, 1).magnitude mustBe 1

    Vec(1, 2, 3).magnitude mustBe Math.sqrt(14)
    Vec(-1, -2, -3).magnitude mustBe Math.sqrt(14)
  }

  it should "properly normalize" in {
    val a = Tuple(4, 0, 0, 0)
    a.normalized mustBe Tuple(1, 0, 0, 0)

    val b = Vec(1, 2, 3)
    val b2 = b.normalized

    b2.x mustBe 1 / Math.sqrt(14)
    b2.y mustBe 2 / Math.sqrt(14)
    b2.z mustBe 3 / Math.sqrt(14)
  }

  it should "properly compute dot products" in {
    val a = Vec(1, 2, 3)
    val b = Vec(2, 3, 4)

    a dot b mustBe 20
  }

  it should "properly cross product" in {
    val a = Vec(1, 2, 3)
    val b = Vec(2, 3, 4)

    a x b mustBe Vec(-1, 2, -1)
    b x a mustBe Vec(1, -2, 1)
  }

}
