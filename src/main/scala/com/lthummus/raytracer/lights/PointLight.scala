package com.lthummus.raytracer.lights

import com.lthummus.raytracer.primitive.{Color, Point, Tuple}

case class PointLight(pos: Tuple, intensity: Color)

object PointLight {
  val Default: PointLight = PointLight(Point(-10, 10, -10), Color(1, 1, 1))
}