package com.lthummus.raytracer.material

import com.lthummus.raytracer.TolerantEquality
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.primitive.{Color, Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SimpleMaterialSpec extends AnyFlatSpec with Matchers with TolerantEquality {

  "SimpleMaterial" should "construct properly" in {
    val c = Color(1, 1, 1)

    val m = SimpleMaterial(c, 0.1, 0.9, 0.9, 200.0)

    m.color mustBe c
    m.ambient mustBe 0.1
    m.diffuse mustBe 0.9
    m.specular mustBe 0.9
    m.shininess mustBe 200.0
  }

  "lighting" should "calculate with light between eye and surface" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin

    val light = PointLight(Point(0, 0, -10), Color.White)
    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(light, pos, eyeVector, normalVector)

    r mustBe Color(1.9, 1.9, 1.9)
  }

  it should "calculate with eye between light and surface, 45 degrees off normal" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin
    val rootTwoOverTwo = Math.sqrt(2) / 2

    val light = PointLight(Point(0, 0, -10), Color.White)
    val eyeVector = Vec(0, rootTwoOverTwo, -rootTwoOverTwo)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(light, pos, eyeVector, normalVector)

    r mustBe Color(1, 1, 1)
  }

  it should "calculate with eye between light and surface, light 45 degrees off normal" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin

    val light = PointLight(Point(0, 10, -10), Color.White)
    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(light, pos, eyeVector, normalVector)

    assert(r === Color(0.73639, 0.73639, 0.73639))
  }

  it should "calculate with eye in path of reflection" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin
    val rootTwoOverTwo = Math.sqrt(2) / 2


    val light = PointLight(Point(0, 10, -10), Color.White)
    val eyeVector = Vec(0, -rootTwoOverTwo, -rootTwoOverTwo)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(light, pos, eyeVector, normalVector)

    assert(r === Color(1.636396, 1.636396, 1.636396))
  }

  it should "calculate with the light behind the surface" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin

    val light = PointLight(Point(0, 0, 10), Color.White)
    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(light, pos, eyeVector, normalVector)

    r mustBe Color(0.1, 0.1, 0.1)
  }
}
