package com.lthummus.raytracer.primitive

import org.scalactic.TolerantNumerics
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class MatrixSpec extends AnyFlatSpec with Matchers {
  private val epilson = 1e-4d

  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(epilson)

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

  it should "compute the determinant of a 2x2 matrix" in {
    val m = Matrix(1, 5, -3, 2)
    m.determinant mustBe 17
  }

  it should "handle making a submatrix" in {
    val m = Matrix(1, 5, 0, -3, 2, 7, 0, 6, -3)
    val correct = Matrix(-3, 2, 0, 6)

    m.submatrix(0, 2) mustBe correct
  }

  it should "handle making a submatrix from a 4x4 matrix" in {
    val m = Matrix(-6, 1, 1, 6, -8, 5, 8, 6, -1, 0, 8, 2, -7, 1, -1, 1)
    val correct = Matrix(-6, 1, 6, -8, 8, 6, -7, -1, 1)

    m.submatrix(2, 1) mustBe correct
  }

  it should "handle making minors" in {
    val m = Matrix(3, 5, 0, 2, -1, -7, 6, -1, 5)
    val sub = m.submatrix(1, 0)

    sub.determinant mustBe 25
    m.minor(1, 0) mustBe 25
  }

  it should "handle cofactors" in {
    val m = Matrix(3, 5, 0, 2, -1, -7, 6, -1, 5)

    m.minor(0, 0) mustBe -12
    m.cofactor(0, 0) mustBe -12
    m.minor(1, 0) mustBe 25
    m.cofactor(1, 0) mustBe -25
  }

  it should "handle determinants of a 3x3 matrix" in {
    val m = Matrix(1, 2, 6, -5, 8, -4, 2, 6, 4)

    m.cofactor(0, 0) mustBe 56
    m.cofactor(0, 1) mustBe 12
    m.cofactor(0, 2) mustBe -46
    m.determinant mustBe -196
  }

  it should "handle determinants of a 4x4 matrix" in {
    val m = Matrix(-2, -8, 3, 5, -3, 1, 7, 3, 1, 2, -9, 6, -6, 7, 7, -9)

    m.cofactor(0, 0) mustBe 690
    m.cofactor(0, 1) mustBe 447
    m.cofactor(0, 2) mustBe 210
    m.cofactor(0, 3) mustBe 51
    m.determinant mustBe -4071
  }

  it should "properly determine invertibility" in {
    val a = Matrix(6, 4, 4, 4, 5, 5, 7, 6, 4, -9, 3, -7, 9, 1, 7, -6)
    val b = Matrix(-4, 2, -2, -3, 9, 6, 2, 6, 0, -5, 1, -5, 0, 0, 0, 0)

    a.determinant mustBe -2120
    b.determinant mustBe 0

    a.isInvertible mustBe true
    b.isInvertible mustBe false

  }

  it should "properly invert a matrix" in {
    val m = Matrix(-5,  2,  6, -8,
                    1, -5,  1,  8,
                    7,  7, -6, -7,
                    1, -3,  7,  4)
    val i = m.inverted

    m.determinant mustBe 532
    m.isInvertible mustBe true

    m.cofactor(2, 3) mustBe -160
    m.cofactor(3, 2) mustBe 105

    i(3, 2) mustBe -160d/532
    i(2, 3) mustBe 105d/532

    assert(i(0, 0) ===  0.21805)
    assert(i(0, 1) ===  0.45113)
    assert(i(0, 2) ===  0.24060)
    assert(i(0, 3) === -0.04511)

    assert(i(1, 0) === -0.80827)
    assert(i(1, 1) === -1.45677)
    assert(i(1, 2) === -0.44361)
    assert(i(1, 3) ===  0.52068)

    assert(i(2, 0) === -0.07895)
    assert(i(2, 1) === -0.22368)
    assert(i(2, 2) === -0.05263)
    assert(i(2, 3) ===  0.19737)

    assert(i(3, 0) === -0.52256)
    assert(i(3, 1) === -0.81391)
    assert(i(3, 2) === -0.30075)
    assert(i(3, 3) ===  0.30639)

  }

  it should "invert another matrix" in {
    val m = Matrix(8, -5, 9, 2, 7, 5, 6, 1, -6, 0, 9, 6, -3, 0, -9, -4)
    val i = m.inverted

    assert(i(0, 0) === -0.15385)
    assert(i(0, 1) === -0.15385)
    assert(i(0, 2) === -0.28205)
    assert(i(0, 3) === -0.53846)

    assert(i(1, 0) === -0.07692)
    assert(i(1, 1) ===  0.12308)
    assert(i(1, 2) ===  0.02564)
    assert(i(1, 3) ===  0.03077)

    assert(i(2, 0) ===  0.35897)
    assert(i(2, 1) ===  0.35897)
    assert(i(2, 2) ===  0.43590)
    assert(i(2, 3) ===  0.92308)

    assert(i(3, 0) === -0.69231)
    assert(i(3, 1) === -0.69231)
    assert(i(3, 2) === -0.76923)
    assert(i(3, 3) === -1.92308)
  }

  it should "invert a third matrix" in {
    val m = Matrix(9, 3, 0, 9, -5, -2, -6, -3, -4, 9, 6, 4, -7, 6, 6, 2)
    val i = m.inverted

    assert(i(0, 0) === -0.04074)
    assert(i(0, 1) === -0.07778)
    assert(i(0, 2) ===  0.14444)
    assert(i(0, 3) === -0.22222)

    assert(i(1, 0) === -0.07778)
    assert(i(1, 1) ===  0.03333)
    assert(i(1, 2) ===  0.36667)
    assert(i(1, 3) === -0.33333)

    assert(i(2, 0) === -0.02901)
    assert(i(2, 1) === -0.14630)
    assert(i(2, 2) === -0.10926)
    assert(i(2, 3) ===  0.12963)

    assert(i(3, 0) ===  0.17778)
    assert(i(3, 1) ===  0.06667)
    assert(i(3, 2) === -0.26667)
    assert(i(3, 3) ===  0.33333)
  }

  it should "ensure that matrix multiplication with inverses work as expected" in {
    val m = Matrix(3, -9, 7, 3, 3, -8, 2, -9, -4, 4, 4, 1, -6, 5, -1, 1)
    val n = Matrix(8, 2, 2, 2, 3, -1, 7, 0, 7, 0, 5, 4, 6, -2, 0, 5)

    val o = m * n
    val p = o * n.inverted

    assert(p.tolerantEqual(m))
  }
}
