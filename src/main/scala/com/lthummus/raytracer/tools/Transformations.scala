package com.lthummus.raytracer.tools

import com.lthummus.raytracer.primitive.{Matrix, Tuple}

object Transformations {
  def translation(x: Double, y: Double, z: Double): Matrix = {
    Matrix(
      1, 0, 0, x,
      0, 1, 0, y,
      0, 0, 1, z,
      0, 0, 0, 1
    )
  }

  def scale(x: Double, y: Double, z: Double): Matrix = {
    Matrix(
      x, 0, 0, 0,
      0, y, 0, 0,
      0, 0, z, 0,
      0, 0, 0, 1
    )
  }

  def rotateX(r: Double): Matrix = {
    Matrix(
      1, 0, 0, 0,
      0, Math.cos(r), -Math.sin(r), 0,
      0, Math.sin(r), Math.cos(r), 0,
      0, 0, 0, 1
    )
  }

  def rotateY(r: Double): Matrix = {
    Matrix(
      Math.cos(r), 0, Math.sin(r), 0,
      0, 1, 0, 0,
      -Math.sin(r), 0, Math.cos(r), 0,
      0, 0, 0, 1
    )
  }

  def rotateZ(r: Double): Matrix = {
    Matrix(
      Math.cos(r), -Math.sin(r), 0, 0,
      Math.sin(r), Math.cos(r), 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1
    )
  }

  def sheer(xy: Double, xz: Double, yx: Double, yz: Double, zx: Double, zy: Double): Matrix = {
    Matrix(
      1, xy, xz, 0,
      yx, 1, yz, 0,
      zx, zy, 1, 0,
      0, 0, 0, 1
    )
  }

  def viewTransform(from: Tuple, to: Tuple, up: Tuple): Matrix = {
    val forward = (to - from).normalized
    val normalizedUp = up.normalized
    val left = forward x normalizedUp
    val trueUp = left x forward

    val orientation = Matrix(
      left.x, left.y, left.z, 0,
      trueUp.x, trueUp.y, trueUp.z, 0,
      -forward.x, -forward.y, -forward.z, 0,
      0, 0, 0, 1
    )

    orientation * translation(-from.x, -from.y, -from.z)
  }
}

object Translate {
  def apply(x: Double, y: Double, z: Double): Matrix = Transformations.translation(x, y, z)
}

object Scale {
  def apply(x: Double, y: Double, z: Double): Matrix = Transformations.scale(x, y, z)
}

object RotateX {
  def apply(r: Double): Matrix = Transformations.rotateX(r)
}

object RotateY {
  def apply(r: Double): Matrix = Transformations.rotateY(r)
}

object RotateZ {
  def apply(r: Double): Matrix = Transformations.rotateZ(r)
}

object Sheer {
  def apply(xy: Double, xz: Double, yx: Double, yz: Double, zx: Double, zy: Double): Matrix =
    Transformations.sheer(xy, xz, yx, yz, zx, zy)
}

object Identity {
  def apply: Matrix = Matrix.Identity4
}

object ViewTransformation {
  def apply(from: Tuple, to: Tuple, up: Tuple): Matrix = Transformations.viewTransform(from, to, up)
}