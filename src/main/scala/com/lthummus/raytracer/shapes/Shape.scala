package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Tuple}
import com.lthummus.raytracer.rays.Ray

abstract class Shape() {

  type T <: Shape
  val transformation: Matrix
  val material: SimpleMaterial

  protected def rayToObjectSpace(r: Ray): Ray = r.transform(transformation.inverted)
  protected def pointToObjectSpace(p: Tuple): Tuple = transformation.inverted * p
  protected def vecToWorldNormalized(v: Tuple): Tuple = (transformation.inverted.transpose * v).copy(w = 0).normalized

  def intersections(r: Ray): Seq[Intersection] = shapeIntersectionFrom(rayToObjectSpace(r))
  def normal(p: Tuple): Tuple = {
    val objectPoint = pointToObjectSpace(p)

    vecToWorldNormalized(shapeNormalAt(objectPoint))
  }
  def reflect(p: Tuple): Tuple = p.reflectVector(normal(p))

  private[shapes] def shapeNormalAt(p: Tuple): Tuple
  private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection]
}