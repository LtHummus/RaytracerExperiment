package com.lthummus.raytracer.camera

import com.lthummus.raytracer.{SpecConstants, TolerantEquality}
import com.lthummus.raytracer.primitive.{Color, Matrix, Point, Vec}
import com.lthummus.raytracer.tools.Transformations
import com.lthummus.raytracer.world.World
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class SimpleCameraSpec extends AnyFlatSpec with Matchers with TolerantEquality with SpecConstants {

  "SimpleCamera" should "construct properly" in {
    val c = SimpleCamera(160, 120, Pi / 2)

    c.horizSize mustBe 160
    c.vertSize mustBe 120
    c.fov mustBe Pi / 2
    c.transform mustBe Matrix.Identity4
  }

  it should "know to calculate pixel size in a horizontal canvas" in {
    val c = SimpleCamera(200, 125, Pi / 2)

    assert(c.pixelSize === 0.01)
  }

  it should "know to calculate pixel size in a vertical canvas" in {
    val c = SimpleCamera(125, 200, Pi / 2)

    assert(c.pixelSize === 0.01)
  }

  "ray for camera" should "handle going through the center of the canvas" in {
    val c = SimpleCamera(201, 101, Pi / 2)

    val r = c.rayForPixel(100, 50)

    r.origin mustBe Point(0, 0, 0)
    assert(r.direction === Vec(0, 0, -1))
  }

  it should "handle going through a corner of the canvas" in {
    val c = SimpleCamera(201, 101, Pi / 2)

    val r = c.rayForPixel(0, 0)

    r.origin mustBe Point(0, 0, 0)
    assert(r.direction === Vec(0.66519, 0.33259, -0.66851))
  }

  it should "handle a ray when the camera is transformed" in {
    val t = Transformations.rotateY(Pi / 4) * Transformations.translation(0, -2, 5)
    val c = SimpleCamera(201, 101, Pi / 2, t)

    val r = c.rayForPixel(100, 50)

    r.origin mustBe Point(0, 2, -5)
    assert(r.direction === Vec(HalfRootTwo, 0, -HalfRootTwo))
  }

  "render" should "work" in {
    val w = World.Default
    val from = Point(0, 0, -5)
    val to = Point(0, 0, 0)
    val up = Vec(0, 1, 0)
    val t = Transformations.viewTransform(from, to, up)

    val c = SimpleCamera(11, 11, Pi / 2, t)

    val render = c.render(w)

    assert(render.getPixel(5, 5) === Color(0.38066, 0.47583, 0.2855))
  }
}
