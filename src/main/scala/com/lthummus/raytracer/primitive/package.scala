package com.lthummus.raytracer

package object primitive {
  implicit class RichDouble(x: Double) {
    def clamp(min: Double, max: Double): Double = {
      if (x < min) {
        min
      } else if (x > max) {
        max
      } else {
        x
      }
    }
  }
}
