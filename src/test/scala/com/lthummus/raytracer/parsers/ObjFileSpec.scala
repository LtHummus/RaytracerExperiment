package com.lthummus.raytracer.parsers

import com.lthummus.raytracer.primitive.{Point, Vec}
import com.lthummus.raytracer.shapes.{SmoothTriangle, Triangle}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ObjFileSpec extends AnyFlatSpec with Matchers {

  "ObjFileParser" should "skip unrecognizable lines" in {
    val data =
      """This is some
        |garbage that the
        |parser should
        |ignore
        |completely
        |""".stripMargin

    val res = ObjFile.fromRawString(data)

    res.skippedLines mustBe 5
    res.vertices must have length 0
    res.parentGroup.children must have length 0
  }

  it should "be able to parse some vertices" in {
    val data =
      """v -1 1 0
        |v -1.000 0.5000 0.000
        |v 1 0 0
        |v 1 1 0
        |""".stripMargin

    val res = ObjFile.fromRawString(data)

    res.skippedLines mustBe 0
    res.vertices must have length 4
    res.parentGroup.children must have length 0

    res.vertices(0) mustBe Point(-1, 1, 0)
    res.vertices(1) mustBe Point(-1, 0.5, 0)
    res.vertices(2) mustBe Point(1, 0, 0)
    res.vertices(3) mustBe Point(1, 1, 0)
  }

  it should "be able to handle triangle faces" in {
    val data =
      """v -1 1 0
        |v -1 0 0
        |v 1 0 0
        |v 1 1 0
        |
        |f 1 2 3
        |f 1 3 4
        |""".stripMargin

    val res = ObjFile.fromRawString(data)

    res.parentGroup.children must have length 2

    val t1 = res.parentGroup.children(0).asInstanceOf[Triangle]
    val t2 = res.parentGroup.children(1).asInstanceOf[Triangle]

    t1.p1 mustBe res.vertices(0)
    t1.p2 mustBe res.vertices(1)
    t1.p3 mustBe res.vertices(2)

    t2.p1 mustBe res.vertices(0)
    t2.p2 mustBe res.vertices(2)
    t2.p3 mustBe res.vertices(3)
  }

  it should "be able to handle more complex polygons" in {
    val data = """v -1 1 0
                 |v -1 0 0
                 |v 1 0 0
                 |v 1 1 0
                 |v 0 2 0
                 |
                 |f 1 2 3 4 5 """.stripMargin

    val res = ObjFile.fromRawString(data)

    val parent = res.parentGroup
    val t1 = parent.children(0).asInstanceOf[Triangle]
    val t2 = parent.children(1).asInstanceOf[Triangle]
    val t3 = parent.children(2).asInstanceOf[Triangle]

    t1.p1 mustBe res.vertices(0)
    t1.p2 mustBe res.vertices(1)
    t1.p3 mustBe res.vertices(2)

    t2.p1 mustBe res.vertices(0)
    t2.p2 mustBe res.vertices(2)
    t2.p3 mustBe res.vertices(3)

    t3.p1 mustBe res.vertices(0)
    t3.p2 mustBe res.vertices(3)
    t3.p3 mustBe res.vertices(4)
  }

  it should "handle group names" in {
    val res = ObjFile.fromResource("triangles.obj")

    res.groupMap must have size 2
    val g1 = res.groupMap("FirstGroup")
    val g2 = res.groupMap("SecondGroup")

    val t1 = g1.children.head.asInstanceOf[Triangle]
    val t2 = g2.children.head.asInstanceOf[Triangle]

    t1.p1 mustBe res.vertices(0)
    t1.p2 mustBe res.vertices(1)
    t1.p3 mustBe res.vertices(2)

    t2.p1 mustBe res.vertices(0)
    t2.p2 mustBe res.vertices(2)
    t2.p3 mustBe res.vertices(3)
  }

  it should "handle normals" in {
    val data =
      """vn 0 0 1
        |vn 0.707 0 -0.707
        |vn 1 2 3
        |""".stripMargin

    val res = ObjFile.fromRawString(data)

    res.normals must have length 3

    res.normals(0) mustBe Vec(0, 0, 1)
    res.normals(1) mustBe Vec(0.707, 0, -0.707)
    res.normals(2) mustBe Vec(1, 2, 3)
  }

  it should "handle normals on faces" in {
    val res = ObjFile.fromResource("triangles_with_normals.obj")

    val g = res.parentGroup
    val t1 = g.children(0).asInstanceOf[SmoothTriangle]
    val t2 = g.children(1).asInstanceOf[SmoothTriangle]

    t1.p1 mustBe res.vertices(0)
    t1.p2 mustBe res.vertices(1)
    t1.p3 mustBe res.vertices(2)
    t1.n1 mustBe res.normals(2)
    t1.n2 mustBe res.normals(0)
    t1.n3 mustBe res.normals(1)

    t2 mustBe t1
  }
}
