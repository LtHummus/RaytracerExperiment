package com.lthummus.raytracer.primitive

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MatrixSpec extends AnyFlatSpec with Matchers {
  "Matrix" should "properly construct" in {
    val m = Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

    m(0, 0) mustBe 1
    m(3, 3) mustBe 16
  }

  it should "work with a more complicated example" in {
    val m = Matrix(1, 2, 3, 4, 5.5, 6.5, 7.5, 8.5, 9, 10, 11, 12, 13.5, 14.5, 15.5, 16.5)

    m(0, 0) mustBe 1
    m(0, 3) mustBe 4
    m(1, 0) mustBe 5.5
    m(1, 2) mustBe 7.5
    m(2, 2) mustBe 11
    m(3, 0) mustBe 13.5
    m(3, 2) mustBe 15.5
  }

  it should "properly update" in {
    val m = Matrix(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

    m(0, 0) mustBe 0
    m(0, 0) = 999
    m(0, 0) mustBe 999
  }

  it should "handle 2x2 matricies" in {
    val m = Matrix(-3, 5, 1, -2)

    m(0, 0) mustBe -3
    m(0, 1) mustBe 5
    m(1, 0) mustBe 1
    m(1, 1) mustBe -2
  }

  it should "handle 3x3 matricies" in {
    val m = Matrix(-3, 5, 0, 1, -2, 7, 0, 1, 1)

    m(0, 0) mustBe -3
    m(0, 1) mustBe 5
    m(0, 2) mustBe 0
    m(1, 0) mustBe 1
    m(1, 1) mustBe -2
    m(1, 2) mustBe 7
    m(2, 0) mustBe 0
    m(2, 1) mustBe 1
    m(2, 2) mustBe 1
  }

  it should "reject non-square matrices" in {
    try {
      val m = Matrix(Array(
        Array(1d),
        Array(1d, 2d)
      ))
      fail("Should have rejected non-square matrix")
    } catch {
      case _: IllegalArgumentException => //nop
    }
  }

  it should "reject a 5x5 matrix" in {
    try {
      Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)
      fail("Should have rejected 5x5 matrix")
    } catch {
      case _: IllegalArgumentException => //nop
    }
  }

  it should "properly recognize same matrices" in {
    val a = Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val b = Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val c = Matrix(2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

    a == b mustBe true
    a == c mustBe false
    b == c mustBe false
    b == a mustBe true
    c == a mustBe false
    c == b mustBe false
  }

  it should "properly extract rows" in {
    val a = Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

    a.row(0) mustBe Array( 1,  2,  3,  4)
    a.row(1) mustBe Array( 5,  6,  7,  8)
    a.row(2) mustBe Array( 9, 10, 11, 12)
    a.row(3) mustBe Array(13, 14, 15, 16)
  }

  it should "properly extract cols" in {
    val a = Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

    a.col(0) mustBe Array(1, 5, 9, 13)
    a.col(1) mustBe Array(2, 6, 10, 14)
    a.col(2) mustBe Array(3, 7, 11, 15)
    a.col(3) mustBe Array(4, 8, 12, 16)
  }

  it should "multiply 4x4 matrices correctly" in {
    val a = Matrix(1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2)
    val b = Matrix(-2, 1, 2, 3, 3, 2, 1, -1, 4, 3, 6, 5, 1, 2, 7, 8)

    val correct = Matrix(20, 22, 50, 48, 44, 54, 114, 108, 40, 58, 110, 102, 16, 26, 46, 42)

    a * b mustBe correct

  }

  it should "multiply by tuples properly" in {
    val m = Matrix(1, 2, 3, 4, 2, 4, 4, 2, 8, 6, 4, 1, 0, 0, 0, 1)
    val t = Tuple(1, 2, 3, 1)

    val correct = Tuple(18, 24, 33, 1)

    m * t mustBe correct
  }

  it should "properly get a 4x4 identity matrix" in {
    val correctIdentity4 = Matrix(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)

    Matrix.Identity4 mustBe correctIdentity4
  }

  it should "multiplication by identity is the same matrix" in {
    val m = Matrix(0, 1, 2, 3, -4, -3, -2, -1, 4, 5, 6, 8, 1, 1, 1, -1)

    val s = m * Matrix.Identity4

    s mustBe m
  }

  it should "multiplication by identity is the same tuple" in {
    val t = Tuple(4, -1, -2, 99)

    val s = Matrix.Identity4 * t

    s mustBe t
  }

  it should "transpose matrices properly" in {
    val m = Matrix(0, 9, 3, 0, 9, 8, 0, 8, 1, 8, 5, 3, 0, 0, 5, 8)

    val correct = Matrix(0, 9, 1, 0, 9, 8, 8, 0, 3, 0, 5, 5, 0, 8, 3, 8)

    m.transpose mustBe correct
  }

  it should "correctly realize that transposing the identity matrix is the identity matrix" in {
    Matrix.Identity4.transpose mustBe Matrix.Identity4
  }
}
