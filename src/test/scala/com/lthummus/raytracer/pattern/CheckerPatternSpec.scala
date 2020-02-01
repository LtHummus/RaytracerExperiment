package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Point}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CheckerPatternSpec extends AnyFlatSpec with Matchers {

  "CheckerPattern" should "repeat in X" in {
    val p = CheckerPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(0.99, 0, 0)) mustBe Color.White
    p.colorAt(Point(1.01, 0, 0)) mustBe Color.Black
  }

  it should "repeat in Y" in {
    val p = CheckerPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(0, 0, 0.99)) mustBe Color.White
    p.colorAt(Point(0, 0, 1.01)) mustBe Color.Black
  }

  it should "repeat in Z" in {
    val p = CheckerPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(0, 0, 0.99)) mustBe Color.White
    p.colorAt(Point(0, 0, 1.01)) mustBe Color.Black
  }
}
