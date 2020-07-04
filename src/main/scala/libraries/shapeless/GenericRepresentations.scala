package libraries.shapeless

import shapeless.{Generic, HNil}

object GenericRepresentations extends App {
  sealed trait Shape
  case class Rect(width: Double, height: Double) extends Shape
  case class Ellipse(major: Double, minor: Double) extends Shape

  // Generic representation of sum types (like Shape)
  val shapeRepresenter: Generic[Shape] =
    Generic[Shape]
  val genericReprOfRect: shapeRepresenter.Repr =
    shapeRepresenter.to(Rect(10,2))
  val genericReprOfEllipse: shapeRepresenter.Repr =
    shapeRepresenter.to(Ellipse(10,2))
  val concreteRect: Shape =
    shapeRepresenter.from(genericReprOfRect)
  val concreteEllipse: Shape =
    shapeRepresenter.from(genericReprOfEllipse)
  println(s"Generic repr of a shape: $genericReprOfRect => $concreteRect")
  println(s"Generic repr of a shape: $genericReprOfEllipse => $concreteEllipse")

  // Generic representation of product types (like Rect or Ellipse)
  val rectRepresenter: Generic[Rect] =
    Generic[Rect]
  val genericReprOfRect2: rectRepresenter.Repr =
    rectRepresenter.to(Rect(3,5))
  val concreteRect2: Rect =
    rectRepresenter.from(genericReprOfRect2)
  println(s"Generic repr of a rect: $genericReprOfRect2 => $concreteRect2")
  println(s"Generic repr of an ellipse: " + Generic[Ellipse].to(Ellipse(3,2)) + " => " + Generic[Ellipse].from(3.0 :: 2.0 :: HNil))
}
