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

  def appendShape(s: Shape): World = {
    objectList.addOne(s)
    this
  }

  def intersections(ray: Ray): Seq[Intersection] = {
    objects.flatMap(_.intersections(ray)).sorted
  }

  def shadeHit(info: IntersectionInformation, lifetime: Int = 5): Color = {
    val surface = info.obj.material.lighting(info.obj, lightSource.get, info.point, info.eyeVector, info.normalVector, isShadowed(info.overPoint))
    val reflected = reflectedColor(info, lifetime)
    val refracted = refractedColor(info, lifetime)

    if (info.obj.material.reflective > 0 && info.obj.material.transparency > 0) {
      val reflectance = info.schlick
      surface + reflected * reflectance + refracted * (1 - reflectance)
    } else {
      surface + reflected + refracted
    }
  }

  def colorAt(ray: Ray, lifetime: Int = 5): Color = {
    val allIntersections = intersections(ray)

    allIntersections.hit match {
      case None      => Color.Black //ray doesn't hit anything
      case Some(hit) => shadeHit(hit.prepareComputation(ray, allIntersections), lifetime)
    }
  }

  def reflectedColor(info: IntersectionInformation, lifetime: Int = 5): Color = {
    if (lifetime <= 0 || info.obj.material.reflective == 0) {
      Color.Black
    } else {
      val reflectedRay = Ray(info.overPoint, info.reflectVector)
      val c = colorAt(reflectedRay, lifetime - 1)

      c * info.obj.material.reflective
    }
  }

  def refractedColor(info: IntersectionInformation, lifetime: Int = 5): Color = {
    if (lifetime <= 0 || info.obj.material.transparency == 0) {
      Color.Black
    } else {
      val nRatio = info.n1 / info.n2
      val cosI = info.eyeVector dot info.normalVector
      val sin2T = nRatio * nRatio * (1 - (cosI * cosI))

      if (sin2T > 1) {
        Color.Black
      } else {
        val cosT = Math.sqrt(1 - sin2T)
        val direction = info.normalVector * (nRatio * cosI - cosT) - info.eyeVector * nRatio
        val refractRay = Ray(info.underPoint, direction)

        colorAt(refractRay, lifetime - 1) * info.obj.material.transparency
      }
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
