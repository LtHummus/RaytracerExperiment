package com.lthummus.raytracer

import com.lthummus.raytracer.primitive.Tuple

package object shapes {

  def reflectVector(in: Tuple, normal: Tuple): Tuple = in - normal * 2 * (in dot normal)

}
