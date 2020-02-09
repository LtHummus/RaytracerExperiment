package com.lthummus.raytracer

import java.io.{File, FileWriter}

import com.lthummus.raytracer.parsers.scene.ParsedScene
import com.lthummus.raytracer.world.World
import com.typesafe.scalalogging.Logger
import javax.imageio.ImageIO
import scopt.OParser

case class RenderOptions(source: Option[File] = None, format: String = "png", output: Option[File] = None, parallel: Boolean = true)

object Main extends App {

  private val Log = Logger("Main")

  private def run(config: RenderOptions): Unit = {
    val sceneSource = scala.io.Source.fromFile(config.source.get)
    val lines = sceneSource.getLines().mkString("\n")
    sceneSource.close()

    val scene = ParsedScene.fromRawText(lines)
    if (scene.errored) {
      Log.warn("Error during parsing")
      System.exit(1)
    }

    val world = scene.toWorld
    Log.info(s"World created. Contains ${world.objectCount} objects and ${world.lights.size} lights")

    Log.info("Beginning render")
    val start = System.currentTimeMillis()
    val render = scene.camera.render(world, config.parallel)
    val duration = System.currentTimeMillis() - start
    Log.info(s"Render complete. Took ${duration}ms")


    config.format match {
      case "png" => ImageIO.write(render.asBufferedImage, "png", config.output.get)
      case "ppm" => val fw = new FileWriter(config.output.get); fw.write(render.asPpm); fw.close()
      case _     => Log.warn("Invalid file format")
    }

    Log.info(s"Wrote output ${config.output.get}")
  }

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
        .text("Output format. Only formats supported are png and ppm"),

      opt[Unit]("single-threaded")
        .action((_, c) => c.copy(parallel = false))
        .text("Render in a single thread only")
    )
  }

  OParser.parse(parser, args, RenderOptions()) match {
    case Some(config) => run(config)
    case None => System.exit(1)
  }
}
