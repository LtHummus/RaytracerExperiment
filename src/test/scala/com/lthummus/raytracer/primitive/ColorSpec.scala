package com.lthummus.raytracer.primitive

import com.lthummus.raytracer.TolerantEquality
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ColorSpec extends AnyFlatSpec with Matchers with TolerantEquality {

  "Color" should "properly construct" in {
    val c = Color(1.1, 2.2, 3.3)

    c.red mustBe 1.1
    c.green mustBe 2.2
    c.blue mustBe 3.3
  }

  it should "properly add" in {
    val c1 = Color(1.1, 2.2, 3.3)
    val c2 = Color(-1, -1, -1)

    val s = c1 + c2
    assert(s.red === .1)
    assert(s.green === 1.2)
    assert(s.blue === 2.3)
  }

  it should "handle another add case" in {
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, .25)

    val s = c1 + c2
    assert(s.red === 1.6)
    assert(s.green === .7)
    assert(s.blue === 1.0)
  }

  it should "properly subtract" in {
    val c1 = Color(0.9, 0.6, 0.75)
    val c2 = Color(0.7, 0.1, .25)

    val s = c1 - c2
    assert(s.red === .2)
    assert(s.green === .5)
    assert(s.blue === .5)
  }

  it should "multiply by a scalar value" in {
    val c = Color(0.2, 0.3, 0.4)
    val s = c * 3

    assert(s.red === .6)
    assert(s.green === .9)
    assert(s.blue === 1.2)
  }

  it should "multiple two colors with hadamard product" in {
    val c1 = Color(1, .2, .4)
    val c2 = Color(.9, 1, .1)

    val s = c1 * c2
    assert(s.red === .9)
    assert(s.green === .2)
    assert(s.blue === 0.04)
  }
}
