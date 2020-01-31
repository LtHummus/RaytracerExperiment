package com.lthummus.raytracer.world

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Color, Intersection, IntersectionInformation, Matrix, Point, Tuple}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.{Shape, Sphere}
import com.lthummus.raytracer.tools.Transformations

import scala.collection.mutable


//todo: should light source be singular? might as well add support for multiple lights...
case class World(private val objectList: mutable.ArrayBuffer[Shape], private var lightSource: Option[PointLight]) {

  def objectCount: Int = objectList.length
  def light: Option[PointLight] = lightSource
  def objects: Seq[Shape] = objectList.toSeq

  def intersections(ray: Ray): Seq[Intersection] = {
    objects.flatMap(_.intersections(ray)).sorted
  }

  def shadeHit(info: IntersectionInformation): Color = {
    info.obj.material.lighting(lightSource.get, info.point, info.eyeVector, info.normalVector, isShadowed(info.overPoint))
  }

  def colorAt(ray: Ray): Color = {
    val allIntersections = intersections(ray)

    allIntersections.hit match {
      case None      => Color.Black //ray doesn't hit anything
      case Some(hit) => shadeHit(hit.prepareComputation(ray))
    }
  }

  def isShadowed(p: Tuple): Boolean = {
    val v = lightSource.get.pos - p
    val distance = v.magnitude
    val direction = v.normalized

    val r = Ray(p, direction)
    intersections(r).hit.exists(_.t < distance)
  }
}

object World {
  def Empty: World = World(mutable.ArrayBuffer.empty[Shape], None)

  def Default: World = {
    val l = PointLight(Point(-10, 10, -10), Color(1, 1, 1))
    val s1Material = SimpleMaterial.Default.copy(color = Color(0.8, 1.0, 0.6), diffuse = 0.7, specular = 0.2)
    val s1 = Sphere(Matrix.Identity4, s1Material)
    val s2 = Sphere(Transformations.scale(0.5, 0.5, 0.5))

    World(mutable.ArrayBuffer(s1, s2), Some(l))
  }

  def create(objects: Seq[Shape], light: PointLight): World = World(mutable.ArrayBuffer(objects: _*), Some(light))

}
