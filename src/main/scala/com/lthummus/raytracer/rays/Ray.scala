package com.lthummus.raytracer.rays

import com.lthummus.raytracer.primitive.{Matrix, Tuple}

case class Ray(origin: Tuple, direction: Tuple) {
  if (origin.isVector || direction.isPoint) {
    throw new IllegalArgumentException(s"Only points can be origins. Only vectors can be directions. Given point = $origin; direction = $direction")
  }

  def pos(t: Double): Tuple = origin + direction * t

  def transform(t: Matrix): Ray = Ray(t * origin, t * direction)

}
