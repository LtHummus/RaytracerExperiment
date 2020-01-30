package com.lthummus.raytracer.util

import com.lthummus.raytracer.primitive.Tuple


//todo: add this to tuple?
object Utils {
  def reflectVector(in: Tuple, normal: Tuple): Tuple = in - normal * 2 * (in dot normal)

}
