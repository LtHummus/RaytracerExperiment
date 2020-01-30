package com.lthummus.raytracer

import com.lthummus.raytracer.primitive.{Color, Tuple}
import org.scalactic.{Equality, TolerantNumerics}

trait TolerantEquality {
  private val epilson = 1e-4d

  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(epilson)

  implicit val tupleEquality = new Equality[Tuple] {
    override def areEqual(a: Tuple, b: Any): Boolean = b match {
      case rhs: Tuple =>
        doubleEquality.areEqual(a.x, rhs.x) &&
          doubleEquality.areEqual(a.y, rhs.y) &&
          doubleEquality.areEqual(a.z, rhs.z) &&
          doubleEquality.areEqual(a.w, rhs.w)
      case _ => false
    }
  }

  implicit val colorEquality = new Equality[Color] {
    override def areEqual(a: Color, b: Any): Boolean = b match {
      case rhs: Color =>
        doubleEquality.areEqual(a.red, rhs.red) &&
          doubleEquality.areEqual(a.green, rhs.green) &&
          doubleEquality.areEqual(a.blue, rhs.blue)
      case _ => false
    }
  }
}
