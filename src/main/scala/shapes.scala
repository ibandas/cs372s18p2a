package edu.luc.cs.laufer.cs372.shapes

sealed trait Shape

//Rectangle case class
case class Rectangle(width: Int, height: Int) extends Shape

//Location case class
case class Location(x: Int, y: Int, shape: Shape) extends Shape {
  require(shape != null, "null shape in location")
}

//Ellipse case class
case class Ellipse(m: Int, n: Int) extends Shape

//Group case class
case class Group(shapes: Shape*) extends Shape
