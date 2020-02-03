package com.lthummus.raytracer

import java.io.File

import com.lthummus.raytracer.camera.SimpleCamera
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.{Material, SimpleMaterial}
import com.lthummus.raytracer.parsers.ObjFile
import com.lthummus.raytracer.pattern.{CheckerPattern, GradientPattern, RingPattern, StripedPattern}
import com.lthummus.raytracer.primitive.{Color, Matrix, Point, Vec}
import com.lthummus.raytracer.shapes.{Cone, Cube, Cylinder, Plane, Sphere}
import com.lthummus.raytracer.tools.{RotateX, RotateY, RotateZ, Scale, Transformations, Translate, ViewTransformation}
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
  //val middleSphereMaterial = SimpleMaterial.Default.copy(color = Color(0.1, 0.1, 0.2), ambient = .2, diffuse = 0.1, specular = 0.8, reflective = 0.9, transparency = 1.0, refractiveIndex = 1.5)
  //val middleSphere = Sphere(Translate(-0.5, 1, 2), middleSphereMaterial)

  //val cube = Cube(Translate(0, 1, -2) * RotateY(math.Pi / 5), SimpleMaterial.Default.copy(ambient = .4, diffuse = 0.01, specular = 0, reflective = 0, transparency = .1, refractiveIndex = 1.0, pattern = Some(CheckerPattern(Color.Red, Color.Blue))))

  //val cylinder = Cylinder(0, 4, closed = true, Scale(1.5, 1.5, 1.5), SimpleMaterial.Default.copy(color = Color(0.1, 0.1, 0.1), ambient = 0.2, diffuse = 0.1, specular = 0.8, reflective = 0.9, transparency = 1.0, refractiveIndex = 1.5))

  val teapot = ObjFile.fromResource("teapot.obj")
    .asGroup
    .copy(transformation = RotateY(math.Pi / 2) * RotateX(-math.Pi / 2) * Translate(0, 0, 0) * Scale(.2, .2, .2))

  teapot.setMaterial(SimpleMaterial.Default.copy(color = Color(0.9, 0.9, 0.7)))

  //.copy(transformation = RotateY(Math.PI / 4) * RotateZ(math.Pi / 4) * Translate(-2, 0, 0) * Scale(.3, .3, .3))

  Log.info("Shapes built")

  //build the world
  val light = PointLight(Point(-10, 4, -5), Color.White)
  val world = World.create(Seq(floor, teapot), light)

  Log.info("World created")
  //camera
  val cameraTransform = ViewTransformation(Point(-7, 3.5, 0), Point(0, 1, 0), Vec(0, 1, 0))
  val camera = SimpleCamera(400, 400, Math.PI / 3, cameraTransform)

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
