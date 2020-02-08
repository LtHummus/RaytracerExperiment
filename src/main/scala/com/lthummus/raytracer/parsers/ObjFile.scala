package com.lthummus.raytracer.parsers

import java.io.File
import java.nio.file.Files

import com.lthummus.raytracer.primitive.{Point, Tuple, Vec}
import com.lthummus.raytracer.shapes.{Group, Shape, SmoothTriangle, Triangle}
import com.typesafe.scalalogging.Logger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

sealed trait ParsedObject
case class ParsedVertex(p: Tuple) extends ParsedObject
case class ParsedNormal(p: Tuple) extends ParsedObject
case class ParsedFace(vertexList: Seq[FaceData]) extends ParsedObject
case class ParsedGroupName(name: String) extends ParsedObject
case class Unknown(l: String) extends ParsedObject
case object EmptyString extends ParsedObject

case class FaceData(vertexNum: Int, textureVertex: Option[Int], normalIndex: Option[Int])
object FaceData {
  def apply(x: String): FaceData = {
    val parts = x.split("/")
    val vertexNum = parts.head.toInt
    val normalIndex = parts.length match {
      case 3 => Some(parts(2).toInt)
      case 1 => None
      case _ => throw new IllegalArgumentException(s"Unknown face input: $x")
    }

    FaceData(vertexNum, None, normalIndex)
  }
}

case class ParseResults(vertices: Seq[Tuple], normals: Seq[Tuple], parentGroup: Group, groupMap: Map[String, Group], skippedLines: Int) {
  def asGroup: Group = parentGroup
}

object ObjFile {

  private val Log = Logger("ObjFileParser")
  private val VertexRegex = "v\\s+([-\\d\\.]+) ([-\\d\\.]+) ([-\\d\\.]+)".r
  private val NormalRegex = "vn\\s+([-\\d\\.]+) ([-\\d\\.]+) ([-\\d\\.]+)".r
  private val FaceRegex = "f\\s+(.*)".r
  private val GroupRegex = "g\\s+(.*)".r
  private val EmptyStringRegex = "^\\s*$".r
  private val CommentRegex = "^#.*$".r

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
      case NormalRegex(a, b, c) => ParsedNormal(Vec(a.toDouble, b.toDouble, c.toDouble))
      case FaceRegex(nums)      => ParsedFace(nums.split(" ").map(FaceData.apply).toSeq)
      case GroupRegex(name)     => ParsedGroupName(name)
      case CommentRegex()       => EmptyString
      case EmptyStringRegex()   => EmptyString
      case _                    => Unknown(line)
    }
  }

  private def fanTriangulation(data: Seq[FaceData], vertices: mutable.ArrayBuffer[Tuple], normals: mutable.ArrayBuffer[Tuple]): Seq[Shape] = {
    val triangles = mutable.ArrayBuffer.empty[Shape]

    val smooth = data.head.normalIndex.isDefined
    val mainVertex = vertices(data.head.vertexNum - 1)
    val mainNormal = data.head.normalIndex.map(i => normals(i - 1))
    data
      .drop(1)
      .sliding(2)
      .foreach{ items =>
        val vertexOne = vertices(items(0).vertexNum - 1)
        val vertexTwo = vertices(items(1).vertexNum - 1)

        if (smooth) {
          val normalOne = normals(items(0).normalIndex.get - 1)
          val normalTwo = normals(items(1).normalIndex.get - 1)
          triangles += SmoothTriangle(mainVertex, vertexOne, vertexTwo, mainNormal.get, normalOne, normalTwo)
        } else {
          triangles += Triangle(mainVertex, vertexOne, vertexTwo)
        }
      }

    triangles.toSeq
  }

  def fromLines(lines: Seq[String]): ParseResults = {
    val vertices = mutable.ArrayBuffer.empty[Tuple]
    val normals = mutable.ArrayBuffer.empty[Tuple]
    val groups = mutable.ArrayBuffer.empty[Shape]
    val groupMap = mutable.HashMap.empty[String, Group]
    var skippedLines = 0

    var lastGroupName = "Unknown"

    lines.foreach{ curr =>
      parseLine(curr) match {
        case ParsedVertex(p) => vertices += p
        case ParsedNormal(n) => normals += n
        case ParsedFace(vertexList) =>
          val tris = fanTriangulation(vertexList, vertices, normals)
          groups ++= tris
          groupMap.put(lastGroupName, Group(tris))
        case ParsedGroupName(name) => lastGroupName = name
        case EmptyString => //nop
        case Unknown(l) => Log.warn(s"Unknown OBJ line: $l"); skippedLines += 1
      }
    }

    Log.info(s"Parsed OBJ file. Found ${vertices.length} vertices ${normals.length} normals and ${groupMap.size} groups. Constructed ${groups.toSeq.length} triangles.")
    val parentGroup = Group(groups.toSeq)
    ParseResults(vertices.toSeq, normals.toSeq, parentGroup, groupMap.toMap, skippedLines)
  }

}
