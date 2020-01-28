package com.lthummus.raytracer.primitive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class CanvasSpec extends AnyFlatSpec with Matchers {

  "Canvas" should "properly initialize with all black pixels" in {
    val c = Canvas(10, 20)
    val pixelArray = c.pixels

    pixelArray must have length 20
    assert(pixelArray.forall(_.length == 10))

    pixelArray.flatten.foreach { c =>
      c.red mustBe 0
      c.green mustBe 0
      c.blue mustBe 0
    }
  }

  it should "properly set pixels" in {
    val c = Canvas(10, 20)

    c.setPixel(0, 0, Color.Red)

    val pixelArray = c.pixels.flatten
    pixelArray.head mustBe Color.Red
    assert(pixelArray.tail.forall(_ == Color.Black))
    c.getPixel(0, 0) mustBe Color.Red
    c.getPixel(4, 4) mustBe Color.Black
  }

  it should "properly write a PPM file with the default canvas" in {
    val c = Canvas(5, 3)

    val ppm = c.asPpm
    val lines = ppm.linesIterator.toArray
    lines(0) mustBe "P3"
    lines(1) mustBe "5 3"
    lines(2) mustBe "255"
    lines(3) mustBe "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
    lines(4) mustBe "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
    lines(5) mustBe "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
  }

  it should "write some pixels then write a ppm" in {
    val c = Canvas(5, 3)
    c.setPixel(0, 0, Color.Red)
    c.setPixel(1, 0, Color(999, 999, 999))
    c.setPixel(4, 2, Color(0, .5, 0))

    val ppm = c.asPpm
    val lines = ppm.linesIterator.toArray
    lines(0) mustBe "P3"
    lines(1) mustBe "5 3"
    lines(2) mustBe "255"
    lines(3) mustBe "255 0 0 255 255 255 0 0 0 0 0 0 0 0 0"
    lines(4) mustBe "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
    lines(5) mustBe "0 0 0 0 0 0 0 0 0 0 0 0 0 128 0"
  }

  it should "handle long lines properly" in {
    val c = Canvas(10, 2)

    for {
      x <- 0 until 10
      y <- 0 until 2
    } {
      c.setPixel(x, y, Color(1, .8, .6))
    }

    val lines = c.asPpm.linesIterator.toArray
    lines(0) mustBe "P3"
    lines(1) mustBe "10 2"
    lines(2) mustBe "255"
    lines(3) mustBe "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
    lines(4) mustBe "153 255 204 153 255 204 153 255 204 153 255 204 153"
    lines(5) mustBe "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
    lines(6) mustBe "153 255 204 153 255 204 153 255 204 153 255 204 153"

    c.asPpm.last mustBe '\n'
  }
}
