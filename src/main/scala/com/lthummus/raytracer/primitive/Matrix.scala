package com.lthummus.raytracer.primitive

import org.scalactic.Equality

case class Matrix(rows: Array[Array[Double]]) {
  val size: Int = rows.length
  if (!rows.forall(_.length == size)) {
    throw new IllegalArgumentException("Only square matrices are supported")
  }
  require(rows.forall(_.length == size))

  def apply(x: Int, y: Int): Double = {
    rows(x)(y)
  }

  def update(x: Int, y: Int, value: Double): Unit = {
    rows(x)(y) = value
  }

  def row(x: Int): Array[Double] = rows(x)
  def col(x: Int): Array[Double] = rows.map(_(x))

  def *(that: Matrix): Matrix = {
    if (this.size != 4 || that.size != 4)
      throw new IllegalArgumentException("Only multiplying 4x4 matrices is allowed")

    val elements = for {
      r <- 0 until size
      c <- 0 until size
    } yield {
      this.row(r)
        .zip(that.col(c))
        .map{ case(a, b) => a * b }
        .sum
    }
    Matrix(elements)
  }

  def *(that: Tuple): Tuple = {
    if (this.size != 4)
      throw new IllegalArgumentException("Only multiplying 4x4 matrices is allowed")

    val elements = rows.map { row =>
      row
        .zip(that.asSeq)
        .map{ case(a, b) => a * b }
        .sum
    }

    Tuple(elements)
  }

  def transpose: Matrix = {
    Matrix((for (c <- 0 until size) yield col(c)).toArray)
  }

  def determinant: Double = {
    if (size == 2) {
      apply(0, 0) * apply(1, 1) - apply(0, 1) * apply(1, 0)
    } else {
      (for (c <- 0 until size) yield {
        apply(0, c) * cofactor(0, c)
      }).sum
    }
  }

  def submatrix(r: Int, c: Int): Matrix = {
    val newElements = rows.zipWithIndex.filter(_._2 != r).map(_._1).map { col =>
      col.zipWithIndex.filter(_._2 != c).map(_._1)
    }

    Matrix(newElements)
  }

  def minor(r: Int, c: Int): Double = submatrix(r, c).determinant
  def cofactor(r: Int, c: Int): Double = {
    val factor = if ((r + c) % 2 == 0) 1 else -1
    minor(r, c) * factor
  }

  def isInvertible: Boolean = determinant != 0

  lazy val inverted: Matrix = {
    if (!isInvertible)
      throw new IllegalArgumentException("Matrix is not invertible")

    val d = determinant
    val cofactors = for {
      r <- 0 until size
      c <- 0 until size
    } yield {
      cofactor(c, r)  //switch arguments to transpose
    }

    Matrix(cofactors.map(_ / d))
  }

  private[primitive] def tolerantEqual(that: Matrix)(implicit tolerance: Equality[Double]): Boolean = {
    this.size == that.size && {
      val pairs = this.rows.zip(that.rows)
      pairs.forall{ case (a, b) =>
        a.zip(b).forall { case (c, d) => tolerance.areEqual(c, d) }
      }
    }
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Matrix =>
        this.rows.length == that.rows.length && {
          val pairs = this.rows.zip(that.rows)
          pairs.forall(x => x._1.sameElements(x._2))
        }
    }
  }

  override def toString: String = {
    rows.map { row =>
      row.map { ele =>
        ele.formatted("%f")
      }.mkString(" | ")
    }.mkString("\n")
  }
}

object Matrix {
  private val ValidSizes = Set(4, 9, 16)

  //TODO: wrap this in something to prevent clients from overriding it?
  val Identity4: Matrix = Matrix(1, 0, 0, 0,
                                 0, 1, 0, 0,
                                 0, 0, 1, 0,
                                 0, 0, 0, 1)

  def apply(elements: IndexedSeq[Double]): Matrix = {
    Matrix(elements: _*)
  }

  def apply(elements: Double*): Matrix = {
    if (!ValidSizes.contains(elements.length)) {
      throw new IllegalArgumentException("Only 2x2, 3x3, and 4x4 matrices are valid")
    }
    val dimension = Math.sqrt(elements.length).toInt
    Matrix(elements.grouped(dimension).map(_.toArray).toArray)
  }
}
