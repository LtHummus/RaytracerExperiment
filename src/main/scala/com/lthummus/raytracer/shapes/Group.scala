package com.lthummus.raytracer.shapes

import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Intersection, Matrix, Tuple, Vec}
import com.lthummus.raytracer.rays.Ray

case class Group(var children: Seq[Shape] = Seq(), transformation: Matrix = Matrix.Identity4, var material: SimpleMaterial = SimpleMaterial.Default, var parent: Option[Shape] = None) extends Shape {
  //on construction, force all of our children to have us as parents
  children.foreach(_.parent = Some(this))

  override type T = Group

  def setMaterial(m: SimpleMaterial): Unit = {
    children.foreach(_.material = m)
  }

  override private[shapes] def shapeNormalAt(p: Tuple): Tuple = {
    ???
  }

  override private[shapes] def shapeIntersectionFrom(r: Ray): Seq[Intersection] = {
    children.flatMap(_.intersections(r)).sorted
  }
}
