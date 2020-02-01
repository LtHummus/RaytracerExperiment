package com.lthummus.raytracer

import java.io.File

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.pattern.{CheckerPattern, RingPattern, StripedPattern}
import com.lthummus.raytracer.primitive.{Color, Matrix, Point, Vec}
import com.lthummus.raytracer.shapes.{Plane, Sphere}
import com.lthummus.raytracer.tools.Transformations
import com.lthummus.raytracer.world.World
import javax.imageio.ImageIO


object TestApp extends App {

  val start = System.currentTimeMillis()

  val floorMaterial = SimpleMaterial.Default.copy(pattern = Some(CheckerPattern(Color(0.5, 0.5, 0.5), Color(.75, .75, .75))))
  val floor = Plane(Matrix.Identity4, floorMaterial)

  //middle sphere
  val middleSphereMaterial = SimpleMaterial.Default.copy(color = Color(0.1, 1, 0.5), diffuse = 0.7, specular = 0.3)
  val middleSphere = Sphere(Transformations.translation(-0.5, 1, 0.5), middleSphereMaterial)

  //right sphere
  val rightSphereMaterial = SimpleMaterial.Default.copy(color = Color(0.5, 1, 1), diffuse = 0.3, specular = 0.9)
  val rightSphereTransform = Transformations.translation(1.5, 0.5, -0.5) * Transformations.scale(0.5, 0.5, 0.5)
  val rightSphere = Sphere(rightSphereTransform, rightSphereMaterial)

  //left sphere
  val leftSphereMaterial = SimpleMaterial.Default.copy(color = Color(1, 0.8, 0.1), diffuse = 0.7, specular = 0.3)
  val leftSphereTransform = Transformations.translation(-1.5, 2.33, 0.75) * Transformations.scale(0.33, 0.33, 0.33) * Transformations.scale(.5, 1.4, 1.1)
  val leftSphere = Sphere(leftSphereTransform, leftSphereMaterial)

  //build the world
  val light = PointLight(Point(0, 10, 0), Color(1, 1, 1))
  val world = World.create(Seq(floor), light)

  //camera
  val cameraTransform = Transformations.viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vec(0, 1, 0))
  val camera = SimpleCamera(800, 800, Math.PI / 3, cameraTransform)

  //render image
  val render = camera.render(world)

  //write it out
  ImageIO.write(render.asBufferedImage, "PNG", new File("t2.png"))

  val duration = System.currentTimeMillis() - start
  println(s"Render completed in $duration milliseconds")
}
