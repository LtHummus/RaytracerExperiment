package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Tuple}
import com.lthummus.raytracer.rays.Ray

abstract class Shape() {

  type T <: Shape
  val transformation: Matrix

  var material: SimpleMaterial
  var parent: Option[Shape]

  protected def rayToObjectSpace(r: Ray): Ray = r.transform(transformation.inverted)
  protected def pointToObjectSpace(p: Tuple): Tuple = transformation.inverted * p
  protected def vecToWorldNormalized(v: Tuple): Tuple = (transformation.inverted.transpose * v).copy(w = 0).normalized

  def intersections(r: Ray): Seq[Intersection] = shapeIntersectionFrom(rayToObjectSpace(r))
  def normal(p: Tuple): Tuple = {
    val localPoint = worldToObject(p)
    val localNormal = shapeNormalAt(localPoint)
    normalToWorld(localNormal)
  }
  def reflect(p: Tuple): Tuple = p.reflectVector(normal(p))

  def worldToObject(p: Tuple): Tuple = {
    val parentManipulatedPoint = parent match {
      case None      => p
      case Some(par) => par.worldToObject(p)
    }

    transformation.inverted * parentManipulatedPoint
  }

  def normalToWorld(v: Tuple): Tuple = {
    val n = (transformation.inverted.transpose * v).copy(w = 0).normalized

    parent match {
      case None      => n
      case Some(par) => par.normalToWorld(n)
    }

  }

  private[shapes] def shapeNormalAt(p: Tuple): Tuple
  private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection]
}