package com.lthummus.raytracer.primitive

import java.awt.image.BufferedImage

case class Canvas(width: Int, height: Int) {
  import Canvas._

  private[primitive] val pixels = Array.fill(height, width)(Color.Black)

  def setPixel(x: Int, y: Int, color: Color): Unit = {
    pixels(y)(x) = color
  }

  def getPixel(x: Int, y: Int): Color = pixels(y)(x)

  def asPpm: String = {
    val lines = pixels.map{ line =>
      val ppmText = line.map(_.asPpmString(ColorMax + 1)).mkString(" ")
      if (ppmText.length > PpmMaxLineLength) {
        ppmText
      } else {
        ppmText
      }
    }

    val fixedLines = lines.map(cutLine)

    s"""$PpmMagicNumber
       |$width $height
       |$ColorMax
       |${fixedLines.mkString("\n")}
       |""".stripMargin
  }

  def asBufferedImage: BufferedImage = {
    val bi = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)

    for {
      x <- 0 until width
      y <- 0 until height
    } {
      bi.setRGB(x, y, pixels(y)(x).asRgbInt)
    }

    bi
  }

  private  def cutLine(s: String): String = {
    (0 to (s.length / PpmMaxLineLength))
      .map((x: Int) => s.slice(PpmMaxLineLength * x, PpmMaxLineLength * (x + 1)))
      .map{ x =>
        if (x.length < PpmMaxLineLength) {
          x
        } else {
          val idxOfLastSpace = x.reverse.indexOf(' ')
          val firstChunk = x.length - idxOfLastSpace
          x.slice(0, firstChunk - 1) + "\n" + x.slice(firstChunk, x.length)
        }
      }
      .mkString("")
  }
}

object Canvas {
  private val PpmMagicNumber = "P3"
  private val ColorMax = 255
  private val PpmMaxLineLength = 70
}