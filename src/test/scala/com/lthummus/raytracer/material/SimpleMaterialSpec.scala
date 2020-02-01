package com.lthummus.raytracer.material

import com.lthummus.raytracer.{SpecConstants, TolerantEquality}
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.pattern.StripedPattern
import com.lthummus.raytracer.primitive.{Color, Point, Vec}
import com.lthummus.raytracer.shapes.Sphere
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SimpleMaterialSpec extends AnyFlatSpec with Matchers with TolerantEquality with SpecConstants {

  "SimpleMaterial" should "construct properly" in {
    val c = Color(1, 1, 1)

    val m = SimpleMaterial(c, 0.1, 0.9, 0.9, 200.0, 0.0, None)

    m.color mustBe c
    m.ambient mustBe 0.1
    m.diffuse mustBe 0.9
    m.specular mustBe 0.9
    m.shininess mustBe 200.0
    m.reflective mustBe 0.0
    m.pattern mustBe None
  }

  "lighting" should "calculate with light between eye and surface" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin

    val light = PointLight(Point(0, 0, -10), Color.White)
    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(Sphere(), light, pos, eyeVector, normalVector, inShadow = false)

    r mustBe Color(1.9, 1.9, 1.9)
  }

  it should "calculate with eye between light and surface, 45 degrees off normal" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin

    val light = PointLight(Point(0, 0, -10), Color.White)
    val eyeVector = Vec(0, HalfRootTwo, -HalfRootTwo)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(Sphere(), light, pos, eyeVector, normalVector, inShadow = false)

    r mustBe Color(1, 1, 1)
  }

  it should "calculate with eye between light and surface, light 45 degrees off normal" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin

    val light = PointLight(Point(0, 10, -10), Color.White)
    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(Sphere(), light, pos, eyeVector, normalVector, inShadow = false)

    assert(r === Color(0.73639, 0.73639, 0.73639))
  }

  it should "calculate with eye in path of reflection" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin


    val light = PointLight(Point(0, 10, -10), Color.White)
    val eyeVector = Vec(0, -HalfRootTwo, -HalfRootTwo)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(Sphere(), light, pos, eyeVector, normalVector, inShadow = false)

    assert(r === Color(1.636396, 1.636396, 1.636396))
  }

  it should "calculate with the light behind the surface" in {
    val m = SimpleMaterial.Default
    val pos = Point.Origin

    val light = PointLight(Point(0, 0, 10), Color.White)
    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)

    val r = m.lighting(Sphere(), light, pos, eyeVector, normalVector, inShadow = false)

    r mustBe Color(0.1, 0.1, 0.1)
  }

  it should "handle surface in shadow" in {
    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))

    val r = SimpleMaterial.Default.lighting(Sphere(), light, Point.Origin, eyeVector, normalVector, inShadow = true)

    assert(r === Color(0.1, 0.1, 0.1))
  }

  it should "handle patterns" in {
    val p = StripedPattern(Color.White, Color.Black)
    val m = SimpleMaterial.Default.copy(ambient = 1.0, diffuse = 0.0, specular = 0.0, pattern = Some(p))
    val l = PointLight(Point(0, 0, -10), Color.White)

    val eyeVector = Vec(0, 0, -1)
    val normalVector = Vec(0, 0, -1)

    m.lighting(Sphere(), l, Point(0.9, 0, 0), eyeVector, normalVector, inShadow = false) mustBe Color.White
    m.lighting(Sphere(), l, Point(1.1, 0, 0), eyeVector, normalVector, inShadow = false) mustBe Color.Black

  }
}
