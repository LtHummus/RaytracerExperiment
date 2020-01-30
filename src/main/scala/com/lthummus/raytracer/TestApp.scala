package com.lthummus.raytracer

import java.io.File

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.primitive.{Color, Point, Vec}
import com.lthummus.raytracer.shapes.Sphere
import com.lthummus.raytracer.tools.Transformations
import com.lthummus.raytracer.world.World
import javax.imageio.ImageIO


object TestApp extends App {

  val start = System.currentTimeMillis()

  val floorTransform = Transformations.scale(10, 0.01, 10)
  val wallMaterial = SimpleMaterial.Default.copy(color = Color(1, 0.9, 0.9), specular = 0)
  val floorSphere = Sphere(floorTransform, wallMaterial)

  val leftWallTransform = Transformations.translation(0, 0, 5) *
    Transformations.rotateY(Math.PI / -4) *
    Transformations.rotateX(Math.PI / 2) *
    Transformations.scale(10, 0.01, 10)

  val leftWallSphere = Sphere(leftWallTransform, wallMaterial)

  val rightWallTransform = Transformations.translation(0, 0, 5) *
    Transformations.rotateY(Math.PI / 4) *
    Transformations.rotateX(Math.PI / 2) *
    Transformations.scale(10, 0.01, 10)

  val rightWallSphere = Sphere(rightWallTransform, wallMaterial)

  val middleSphereMaterial = SimpleMaterial.Default.copy(color = Color(0.1, 1, 0.5), diffuse = 0.7, specular = 0.3)
  val middleSphere = Sphere(Transformations.translation(-0.5, 1, 0.5), middleSphereMaterial)

  val rightSphereMaterial = SimpleMaterial.Default.copy(color = Color(0.5, 1, 1), diffuse = 0.3, specular = 0.9)
  val rightSphereTransform = Transformations.translation(1.5, 0.5, -0.5) * Transformations.scale(0.5, 0.5, 0.5)
  val rightSphere = Sphere(rightSphereTransform, rightSphereMaterial)

  val leftSphereMaterial = SimpleMaterial.Default.copy(color = Color(1, 0.8, 0.1), diffuse = 0.7, specular = 0.3)
  val leftSphereTransform = Transformations.translation(-1.5, 0.33, 0.75) * Transformations.scale(0.33, 0.33, 0.33)
  val leftSphere = Sphere(leftSphereTransform, leftSphereMaterial)

  val light = PointLight(Point(-10, 10, -10), Color(1, 1, 1))
  val world = World.create(Seq(floorSphere, leftWallSphere, rightWallSphere, middleSphere, leftSphere, rightSphere), light)

  val cameraTransform = Transformations.viewTransform(Point(0, 1.5, -5), Point(0, 1, 0), Vec(0, 1, 0))
  val camera = SimpleCamera(800, 600, Math.PI / 3, cameraTransform)

  val render = camera.render(world)

  ImageIO.write(render.asBufferedImage, "PNG", new File("t2.png"))

  val duration = System.currentTimeMillis() - start
  println(s"Duration ${duration}ms")
}
