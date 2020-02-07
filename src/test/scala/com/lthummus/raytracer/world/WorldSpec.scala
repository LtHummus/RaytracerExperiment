package com.lthummus.raytracer.world

import com.lthummus.raytracer.{SpecConstants, TolerantEquality, primitive}
import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.material.SimpleMaterial
import com.lthummus.raytracer.pattern.{Pattern, SamplePattern}
import com.lthummus.raytracer.primitive.{Color, Intersection, Matrix, Point, Vec}
import com.lthummus.raytracer.rays.Ray
import com.lthummus.raytracer.shapes.{Plane, Sphere}
import com.lthummus.raytracer.tools.{Transformations, Translate}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import scala.collection.mutable

class WorldSpec extends AnyFlatSpec with Matchers with TolerantEquality with SpecConstants {

  "World" should "be able to be initialized with an empty world" in {
    val w = World.Empty

    w.lights must have length 0
    w.objectCount mustBe 0
  }

  it should "be able to construct a default world" in {
    val w = World.Default

    w.objectCount mustBe 2
    val s1 = w.objects.head
    val s2 = w.objects(1)

    s1.material.color mustBe Color(0.8, 1.0, 0.6)
    s1.material.diffuse mustBe 0.7
    s1.material.specular mustBe 0.2
    s1.transformation mustBe Matrix.Identity4

    s2.transformation mustBe Transformations.scale(0.5, 0.5, 0.5)
    s2.material mustBe SimpleMaterial.Default


    assert(w.lights.nonEmpty)
    assert(w.lights.head.pos == Point(-10, 10, -10))

  }

  "intersection" should "be able to find intersections in the world" in {
    val w = World.Default
    val intersections = w.intersections(Ray(Point(0, 0, -5), Vec(0, 0, 1)))

    intersections must have length 4

    intersections(0).t mustBe 4
    intersections(1).t mustBe 4.5
    intersections(2).t mustBe 5.5
    intersections(3).t mustBe 6
  }

  "shadeHit" should "be able to figure out a color" in {
    val w = World.Default
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val s = w.objects.head
    val i = Intersection(4, s)

    val info = i.prepareComputation(r, Seq(i))

    assert(w.shadeHit(info) === Color(0.38066, 0.47583, 0.2855))
  }

  it should "also be able to handle intersections from the inside" in {
    val w = World.Default.copy(lightArray = mutable.ArrayBuffer(PointLight(Point(0, 0.25, 0), Color(1, 1, 1))))
    val r = Ray(Point(0, 0, 0), Vec(0, 0, 1))
    val s = w.objects(1)
    val i = Intersection(0.5, s)

    val info = i.prepareComputation(r, Seq(i))

    assert(w.shadeHit(info) === Color(0.90498, 0.90498, 0.90498))
  }

  it should "properly recognize shadows" in {
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))
    val s1 = Sphere()
    val s2 = Sphere(Transformations.translation(0, 0, 10))

    val w = World.create(Seq(s1, s2), light)

    val r = Ray(Point(0, 0, 5), Vec(0, 0, 1))
    val i = Intersection(4, s2)

    val info = i.prepareComputation(r, Seq(i))

    w.shadeHit(info) mustBe Color(0.1, 0.1, 0.1)
  }

  it should "handle a reflective material" in {
    val m = SimpleMaterial.Default.copy(reflective = 0.5)
    val s = Plane(Transformations.translation(0, -1, 0), m)
    val w = World.Default.appendShape(s)

    val r = Ray(Point(0, 0, -3), Vec(0, -HalfRootTwo, HalfRootTwo))
    val i = Intersection(RootTwo, s)

    val info = i.prepareComputation(r, Seq(i))

    assert(w.shadeHit(info) === Color(0.87677, 0.92436, 0.82918))
  }

  it should "be able to  handle a transparent material" in {
    val f = Plane(Translate(0, -1, 0), SimpleMaterial.Default.copy(transparency = 0.5, refractiveIndex = 1.5))
    val ball = Sphere(Translate(0, -3.5, -.05), SimpleMaterial.Default.copy(color = Color(1, 0, 0), ambient = 0.5))

    val w = World.Default.appendShape(ball).appendShape(f)

    val r = Ray(Point(0, 0, -3), Vec(0, -HalfRootTwo, HalfRootTwo))
    val xs = Seq(Intersection(RootTwo, f))

    val info = xs.head.prepareComputation(r, xs)

    assert(w.shadeHit(info) === Color(0.93642, 0.68642, 0.68642))
  }

  it should "handle a reflective, transparent material" in {
    val f = Plane(Translate(0, -1, 0), SimpleMaterial.Default.copy(reflective = 0.5, transparency = 0.5, refractiveIndex = 1.5))
    val ball = Sphere(Translate(0, -3.5, -0.5), SimpleMaterial.Default.copy(color = Color(1, 0, 0), ambient = 0.5))
    val w = World.Default.appendShape(f).appendShape(ball)

    val r = Ray(Point(0, 0, -3), Vec(0, -HalfRootTwo, HalfRootTwo))
    val xs = Seq(Intersection(RootTwo, f))

    val info = xs.head.prepareComputation(r, xs)

    assert(w.shadeHit(info) === Color(0.9339158, 0.69643513, 0.6924312))
  }

  "colorAt" should "be able to handle when ray misses" in {
    val w = World.Default
    val r = Ray(Point(0, 0, -5), Vec(0, 1, 0))

    assert(w.colorAt(r) === Color(0, 0, 0))
  }

  it should "determine color when a ray hits" in {
    val w = World.Default
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))

    assert(w.colorAt(r) === Color(0.38066, 0.47583, 0.2855))
  }

  it should "determine color when a ray hits with intersection behind ray" in {
    val w = World.Default
    val outer = w.objects(0).asInstanceOf[Sphere]
    val inner = w.objects(1).asInstanceOf[Sphere]

    val newOuter = outer.copy(material = outer.material.copy(ambient = 1.0))
    val newInner = inner.copy(material = inner.material.copy(ambient = 1.0))
    val newWorld = World(mutable.ArrayBuffer(newOuter, newInner), mutable.ArrayBuffer(PointLight(Point(-10, 10, -10), Color(1, 1, 1))))

    val r = Ray(Point(0, 0, 0.75), Vec(0, 0, -1))
    assert(newWorld.colorAt(r) === newInner.material.color)
  }

  it should "work with two parallel reflective surfaces" in {
    val m = SimpleMaterial.Default.copy(reflective = 1)
    val lower = Plane(Transformations.translation(0, -1, 0), m)
    val upper = Plane(Transformations.translation(0, 1, 0), m)

    val w = World.create(Seq(lower, upper), PointLight(Point.Origin, Color.White))

    val r = Ray(Point(0, 0, 0), Vec(0, 1, 0))

    w.colorAt(r)

    assert("This didn't infinite loop" === "This didn't infinite loop")
  }

  it should "have some reflected color at max recursion" in {
    val m = SimpleMaterial.Default.copy(reflective = .5)
    val s = Plane(Transformations.translation(0, -1, 0), m)
    val w = World.Default.appendShape(s)

    val r = Ray(Point(0, 0, -3), Vec(0, -HalfRootTwo, HalfRootTwo))
    val i = Intersection(RootTwo, s)

    val info = i.prepareComputation(r, Seq(i))
    w.reflectedColor(info, lifetime = 0) mustBe Color.Black
  }


  it should "work with multiple light sources" in {
    val a = Sphere()
    val l1 = PointLight(Point(-5, 10, -5), Color.Red)
    val l2 = PointLight(Point(-5, 10, 5), Color.Blue)

    val w = World.create(Seq(a), Seq(l1, l2))

    assert(w.colorAt(Ray(Point(-5, 1, 0), Vec(1, 0, 0))) === Color(0.8077, 0.0, 0.8077))
  }

  "isShadowed" should "say no shadow when nothing is between point and light" in {
    val w = World.Default
    val p = Point(0, 10, 0)

    w.isShadowed(w.lights.head, p) mustBe false
  }

  it should "say yes shadow when object between point and light" in {
    val w = World.Default
    val p = Point(10, -10, 10)

    w.isShadowed(w.lights.head, p) mustBe true
  }

  it should "say no shadow when object behind light" in {
    val w = World.Default
    val p = Point(-20, 20, -20)

    w.isShadowed(w.lights.head, p) mustBe false
  }

  it should "say no shadow when point between object and light" in {
    val w = World.Default
    val p = Point(-2, 2, -2)

    w.isShadowed(w.lights.head, p) mustBe false
  }

  "reflections" should "figure out the reflected color for a nonreflective material" in {
    val w = World.Default
    val r = Ray(Point(0, 0, 0), Vec(0, 0, 1))
    val s1 = w.objects(0).asInstanceOf[Sphere]
    val s2 = w.objects(1).asInstanceOf[Sphere]
    val newS2 = s2.copy(material = s2.material.copy(ambient = 1.0))
    val newWorld = w.copy(objectList = mutable.ArrayBuffer(s1, newS2))
    val i = Intersection(1, newS2)

    val info = i.prepareComputation(r, Seq(i))

    newWorld.reflectedColor(info) mustBe Color(0, 0, 0)
  }

  it should "work for a reflected material" in {
    val m = SimpleMaterial.Default.copy(reflective = 0.5)
    val s = Plane(Transformations.translation(0, -1, 0), m)

    val w = World.Default.appendShape(s)

    val r = Ray(Point(0, 0, -3), Vec(0, -HalfRootTwo, HalfRootTwo))
    val i = Intersection(RootTwo, s)

    val info = i.prepareComputation(r, Seq(i))

    assert(w.reflectedColor(info) === Color(0.19032, 0.2379, 0.14274))
  }

  "refractions" should "work with opaque surface" in {
    val w = World.Default
    val s = w.objects.head
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val xs = Seq(Intersection(4, s), Intersection(6, s))

    val info = xs.head.prepareComputation(r, xs)

    w.refractedColor(info) mustBe Color.Black
  }

  it should "work at max recursion depth" in {
    val w = World.Default
    val s = w.objects.head.asInstanceOf[Sphere]
    val m = s.material

    val newS = s.copy(material = m.copy(transparency = 1.0, refractiveIndex = 1.5))
    val w2 = World.create(Seq(newS, w.objects(1)), w.lights.head)

    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val xs = Seq(Intersection(4, newS), Intersection(6, newS))
    val info = xs.head.prepareComputation(r, xs)

    w.refractedColor(info, 0) mustBe Color.Black

  }

  it should "work with total internal reflection" in {
    val w = World.Default
    val s = w.objects.head.asInstanceOf[Sphere]
    val m = s.material

    val newS = s.copy(material = m.copy(transparency = 1.0, refractiveIndex = 1.5))
    val w2 = World.create(Seq(newS, w.objects(1)), w.lights.head)

    val r = Ray(Point(0, 0, HalfRootTwo), Vec(0, 1, 0))
    val xs = Seq(
      Intersection(-HalfRootTwo, s),
      Intersection(HalfRootTwo, s)
    )

    val info = xs(1).prepareComputation(r, xs)

    w.refractedColor(info) mustBe Color.Black
  }

  it should "work with a refracted ray" in {
    val a = Sphere(Matrix.Identity4, World.Default.objects.head.material.copy(ambient = 1.0, pattern = Some(SamplePattern())))
    val b = Sphere(Transformations.scale(0.5, 0.5, 0.5), SimpleMaterial.Default.copy(transparency = 1.0, refractiveIndex = 1.5))

    val w = World.create(Seq(a, b), World.Default.lights.head)
    val r = Ray(Point(0, 0, 0.1), Vec(0, 1, 0))

    val xs = Seq(
      Intersection(-0.9899, a),
      Intersection(-0.4899, b),
      Intersection(0.4899, b),
      Intersection(0.9899, a)
    )

    val info = xs(2).prepareComputation(r, xs)
    assert(w.refractedColor(info) === Color(0, 0.9988, 0.04725))
  }
}
