package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Point}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RingPatternSpec extends AnyFlatSpec with Matchers {

  "RingPattern" should "you know ... make rings" in {
    val p = RingPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(1, 0, 0)) mustBe Color.Black
    p.colorAt(Point(0, 0, 1)) mustBe Color.Black
    p.colorAt(Point(0.708, 0, 0.708)) mustBe Color.Black
  }
}
