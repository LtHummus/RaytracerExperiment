package com.lthummus.raytracer

import java.io.{File, FileWriter}

import com.lthummus.raytracer.parsers.scene.ParsedScene
import com.lthummus.raytracer.world.World
import com.typesafe.scalalogging.Logger
import javax.imageio.ImageIO
import scopt.OParser

case class RenderOptions(source: Option[File] = None, format: String = "png", output: Option[File] = None)

object Main extends App {

  private def run(config: RenderOptions): Unit = {
    val sceneSource = scala.io.Source.fromFile(config.source.get)
    val lines = sceneSource.getLines().mkString("\n")
    sceneSource.close()

    val scene = ParsedScene.fromRawText(lines)
    val world = World.create(scene.objects, scene.lights)
    val render = scene.camera.render(world)

    config.format match {
      case "png" => ImageIO.write(render.asBufferedImage, "png", config.output.get)
      case "ppm" => val fw = new FileWriter(config.output.get); fw.write(render.asPpm); fw.close()
      case _     => Log.warn("Invalid file format")
    }
  }

  private val Log = Logger("Main")
  val builder = OParser.builder[RenderOptions]

  val parser = {
    import builder._
    OParser.sequence(
      programName("raytraceExperiment"),
      head("raytraceexperiment", "0.0.1"),

      opt[File]('s', "source")
        .required()
        .action((x, c) => c.copy(source = Some(x)))
        .text("Scene definition file to render"),

      opt[File]('o', "output")
        .required()
        .action((x, c) => c.copy(output = Some(x)))
        .text("Filename of output"),

      opt[String]('f', "format")
        .optional()
        .action((x, c) => c.copy(format = x))
        .validate {
          case "png" => success
          case "ppm" => success
          case _     => failure("format must be `png` or `ppm`")
        }
        .text("Output format. Only formats supported are png and ppm")
    )
  }

  OParser.parse(parser, args, RenderOptions()) match {
    case Some(config) => run(config)
    case None => //nop
  }
}
