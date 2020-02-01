package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Point}
import com.lthummus.raytracer.shapes.Sphere
import com.lthummus.raytracer.tools.Transformations
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class StripedPatternSpec extends AnyFlatSpec with Matchers {

  "StripedPattern" should "be able to be created" in {
    val p = StripedPattern(Color.White, Color.Black)

    p.a mustBe Color.White
    p.b mustBe Color.Black
  }

  it should "be constant in y" in {
    val p = StripedPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(0, 1, 0)) mustBe Color.White
    p.colorAt(Point(0, 2, 0)) mustBe Color.White
  }

  it should "be constant in z" in {
    val p = StripedPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(0, 0, 1)) mustBe Color.White
    p.colorAt(Point(0, 0, 2)) mustBe Color.White
  }

  it should "alternate in x" in {
    val p = StripedPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(0.9, 0, 0)) mustBe Color.White
    p.colorAt(Point(1, 0, 0)) mustBe Color.Black
    p.colorAt(Point(-0.1, 0, 0)) mustBe Color.Black
    p.colorAt(Point(-1, 0, 0)) mustBe Color.Black
    p.colorAt(Point(-1.1, 0, 0)) mustBe Color.White
  }

  "stripeAtObject" should "be able to handle object transforms" in {
    val p = StripedPattern(Color.White, Color.Black)
    val s = Sphere(Transformations.scale(2, 2, 2))

    p.colorOnObject(Point(1.5, 0, 0), s) mustBe Color.White
  }

  it should "be able to handle pattern transforms" in {
    val s = Sphere()
    val p = StripedPattern(Color.White, Color.Black, Transformations.scale(2, 2, 2))

    p.colorOnObject(Point(1.5, 0, 0), s) mustBe Color.White
  }

  it should "be able to handle both pattern AND object transforms" in {
    val s = Sphere(Transformations.scale(2, 2, 2))
    val p = StripedPattern(Color.White, Color.Black, Transformations.translation(0.5, 0, 0))

    p.colorOnObject(Point(2.5, 0, 0), s) mustBe Color.White
  }
}
