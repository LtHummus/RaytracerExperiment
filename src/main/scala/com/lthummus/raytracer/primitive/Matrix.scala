package com.lthummus.raytracer.primitive

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
