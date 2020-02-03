package com.lthummus.raytracer.parsers

import java.io.File
import java.nio.file.Files

import com.lthummus.raytracer.primitive.{Point, Tuple}
import com.lthummus.raytracer.shapes.{Group, Shape, Triangle}
import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import scala.jdk.CollectionConverters._

sealed trait ParsedObject
case class ParsedVertex(p: Tuple) extends ParsedObject
case class ParsedFace(vertexList: Seq[Int]) extends ParsedObject
case class ParsedGroupName(name: String) extends ParsedObject
case class Unknown(l: String) extends ParsedObject
case object EmptyString extends ParsedObject

case class ParseResults(vertices: Seq[Tuple], parentGroup: Group, groupMap: Map[String, Group], skippedLines: Int) {
  def asGroup: Group = parentGroup
}

object ObjFile {

  private val Log = Logger("ObjFileParser")
  private val VertexRegex = "v\\s+([-\\d\\.]+) ([-\\d\\.]+) ([-\\d\\.]+)".r
  private val FaceRegex = "f\\s+(.*)".r
  private val GroupRegex = "g\\s+(.*)".r
  private val EmptyStringRegex = "^\\s+$".r

  def fromRawString(contents: String): ParseResults = fromLines(contents.linesIterator.toSeq)
  def fromFile(file: File): ParseResults = {
    val lines = Files.readAllLines(file.toPath)
    fromLines(lines.asScala.toSeq)
  }
  def fromResource(name: String): ParseResults = {
    val source = scala.io.Source.fromResource(name)
    val lines = source.getLines().toSeq
    source.close()

    fromLines(lines)
  }

  private def parseLine(line: String): ParsedObject = {
    line match {
      case VertexRegex(a, b, c) => ParsedVertex(Point(a.toDouble, b.toDouble, c.toDouble))
      case FaceRegex(nums) => ParsedFace(nums.split(" ").map(_.split("/")(0).toInt).toSeq)
      case GroupRegex(name) => ParsedGroupName(name)
      case EmptyStringRegex() => EmptyString
      case _ => Unknown(line)
    }
  }

  private def fanTriangulation(vertices: Seq[Tuple]): Seq[Triangle] = {
    val triangles = mutable.ArrayBuffer.empty[Triangle]

    vertices
      .drop(1)
      .sliding(2)
      .foreach{ items =>
        triangles += Triangle(vertices.head, items(0), items(1))
      }

    triangles.toSeq
  }

  def fromLines(lines: Seq[String]): ParseResults = {
    val vertices = mutable.ArrayBuffer.empty[Tuple]
    val groups = mutable.ArrayBuffer.empty[Shape]
    val groupMap = mutable.HashMap.empty[String, Group]
    var skippedLines = 0

    var lastGroupName = "Unknown"

    lines.foreach{ curr =>
      parseLine(curr) match {
        case ParsedVertex(p) => vertices += p
        case ParsedFace(vertexList) =>
          val points = vertexList.map(i => vertices(i - 1))
          val tris = fanTriangulation(points)
          groups ++= tris
          groupMap.put(lastGroupName, Group(tris))
        case ParsedGroupName(name) => lastGroupName = name
        case EmptyString => //nop
        case Unknown(l) => Log.warn(s"Unknown OBJ line: $l"); skippedLines += 1
      }
    }

    Log.info(s"Parsed OBJ file. Found ${vertices.length} vertices and ${groupMap.size} groups. Constructed ${groups.toSeq.length} triangles.")
    val parentGroup = Group(groups.toSeq)
    ParseResults(vertices.toSeq, parentGroup, groupMap.toMap, skippedLines)
  }

}
