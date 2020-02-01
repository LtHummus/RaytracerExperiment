package com.lthummus.raytracer.pattern

import com.lthummus.raytracer.primitive.{Color, Point}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class GradientPatternSpec extends AnyFlatSpec with Matchers {

  "GradientPattern" should "linearly interpolate colors" in {
    val p = GradientPattern(Color.White, Color.Black)

    p.colorAt(Point(0, 0, 0)) mustBe Color.White
    p.colorAt(Point(0.25, 0, 0)) mustBe Color(0.75, 0.75, 0.75)
    p.colorAt(Point(0.5, 0, 0)) mustBe Color(0.5, 0.5, 0.5)
    p.colorAt(Point(0.75, 0, 0)) mustBe Color(0.25, 0.25, 0.25)
  }
}
