package com.lthummus.raytracer

import java.io.File

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.pattern.{CheckerPattern, RingPattern, StripedPattern}
import com.lthummus.raytracer.primitive.{Color, Matrix, Point, Vec}
import com.lthummus.raytracer.shapes.{Plane, Sphere}
import com.lthummus.raytracer.tools.{Scale, Transformations, Translate, ViewTransformation}
import com.lthummus.raytracer.world.World
import com.typesafe.scalalogging.Logger
import javax.imageio.ImageIO


object TestApp extends App {

  val Log = Logger("main")

  val start = System.currentTimeMillis()

  Log.info("Constructing world")

  val floorMaterial = SimpleMaterial.Default.copy(specular = 0.5, diffuse = 0.4, reflective = .2, pattern = Some(CheckerPattern(Color(0.5, 0.5, 0.5), Color(.75, .75, .75))))
  val floor = Plane(Matrix.Identity4, floorMaterial)

  Log.info("Floor built")

  //middle sphere
  val middleSphereMaterial = SimpleMaterial.Default.copy(color = Color(0.1, 1, 0.5), diffuse = 0.7, specular = 0.3)
  val middleSphere = Sphere(Translate(-0.5, 1, 0.5), middleSphereMaterial)

  //right sphere
  val rightSphereMaterial = SimpleMaterial.Default.copy(color = Color(0.5, 1, 1), diffuse = 0.3, specular = 0.9)
  val rightSphereTransform = Translate(1.5, 0.5, -0.5) * Scale(0.5, 0.5, 0.5)
  val rightSphere = Sphere(rightSphereTransform, rightSphereMaterial)

  //left sphere
  val leftSphereMaterial = SimpleMaterial.Default.copy(color = Color(1, 0.8, 0.1), diffuse = 0.7, specular = 0.3)
  val leftSphereTransform = Translate(-1.5, 2.33, 0.75) * Scale(0.33, 0.33, 0.33) * Scale(.5, 1.4, 1.1)
  val leftSphere = Sphere(leftSphereTransform, leftSphereMaterial)

  Log.info("Spheres built")

  //build the world
  val light = PointLight(Point(-10, 10, -5), Color.White)
  val world = World.create(Seq(floor, middleSphere, leftSphere, rightSphere), light)

  Log.info("World created")
  //camera
  val cameraTransform = ViewTransformation(Point(0, 1.5, -5), Point(0, 1, 0), Vec(0, 1, 0))
  val camera = SimpleCamera(800, 800, Math.PI / 3, cameraTransform)

  Log.info("Camera built")

  Log.info("Starting render")
  //render image
  val render = camera.render(world)

  val renderTime = System.currentTimeMillis() - start
  Log.info(s"Render completed in ${renderTime}ms")
  //write it out
  ImageIO.write(render.asBufferedImage, "PNG", new File("t2.png"))
  Log.info(s"Image `t2.png` written")
}
