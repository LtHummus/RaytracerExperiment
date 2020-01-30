package com.lthummus.raytracer.material

import com.lthummus.raytracer.lights.PointLight
import com.lthummus.raytracer.primitive.{Color, Tuple}

case class SimpleMaterial(color: Color, ambient: Double, diffuse: Double, specular: Double, shininess: Double) {
  def lighting(light: PointLight, pos: Tuple, eyeVector: Tuple, normalVector: Tuple, inShadow: Boolean): Color = {
    val effectiveColor = color * light.intensity
    val lightVector = (light.pos - pos).normalized
    val ambientLighting = effectiveColor * ambient
    val lightDotNormal = lightVector dot normalVector


    val (d, s) = if (inShadow || lightDotNormal < 0) {
      //light is on other side of surface
      (Color.Black, Color.Black)
    } else {
      val diffuseContrib = effectiveColor * diffuse * lightDotNormal
      val reflectVector = -lightVector.reflectVector(normalVector)
      val reflectDotEye = reflectVector dot eyeVector

      val specularContrib = if (reflectDotEye <= 0) {
        Color.Black
      } else {
        val f = Math.pow(reflectDotEye, shininess)
        light.intensity * specular * f
      }

      (diffuseContrib, specularContrib)
    }

    ambientLighting + d + s
  }
}

object SimpleMaterial {
  val Default: SimpleMaterial = SimpleMaterial(Color(1, 1, 1), 0.1, 0.9, 0.9, 200.0)
}