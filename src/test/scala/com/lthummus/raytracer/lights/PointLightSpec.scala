package com.lthummus.raytracer.lights

import com.lthummus.raytracer.primitive.{Color, Point}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class PointLightSpec extends AnyFlatSpec with Matchers {

  "PointLight" should "construct properly" in {
    val i = Color(1, 1, 1)
    val p = Point(0, 0, 0)

    val l = PointLight(p, i)

    l.intensity mustBe i
    l.pos mustBe p
  }
}
