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
