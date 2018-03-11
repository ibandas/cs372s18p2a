package edu.luc.cs.laufer.cs372.shapes

import org.scalatest.FunSuite

import TestFixtures._

class TestBoundingBox extends FunSuite {

  //Test BoundingBox Function
  def testBoundingBox(description: String, s: Shape, x: Int, y: Int, width: Int, height: Int) = {
    test(description) {
      val Location(u, v, Rectangle(w, h)) = boundingBox(s)
      assert(x === u)
      assert(y === v)
      assert(width === w)
      assert(height === h)
    }
  }

  //Test Scale Function
  def testScale(description: String, s: Shape, factor: Int, expected: Shape) = {
    test(description) {
      val scaledShape = boundingBox.scale(s, factor)
      assert(scaledShape === expected)
    }
  }

  //Test Size Function
  def testSize(description: String, s: Shape, expected: Int) = {
    test(description) {
      val sizeCount = boundingBox.size(s)
      assert(sizeCount === expected)
    }
  }

  //Test Height Function
  def testHeight(description: String, s: Shape, expected: Int) = {
    test(description) {
      val heightCount = boundingBox.height(s)
      assert(heightCount === expected)
    }
  }

  //Bounding Box
  testBoundingBox("simple ellipse", simpleEllipse, -50, -30, 100, 60)
  testBoundingBox("simple rectangle", simpleRectangle, 0, 0, 80, 120) //orig
  testBoundingBox("simple location", simpleLocation, 70, 30, 80, 120) //orig
  testBoundingBox("basic group", basicGroup, -50, -30, 100, 70)
  testBoundingBox("simple group", simpleGroup, 150, 70, 350, 280)
  testBoundingBox("complex group", complexGroup, 30, 60, 470, 320)

  //Scale
  testScale("simple rectangle times 2", simpleRectangle, 2, Rectangle(160, 240))
  testScale("simple rectangle times 3", simpleRectangle, 3, Rectangle(240, 360))
  testScale("simple ellipse times 2", simpleEllipse, 2, Ellipse(100, 60))
  testScale("simple ellipse times 3", simpleEllipse, 3, Ellipse(150, 90))
  testScale("simple group times 2", simpleGroup, 2, Group(Location(400, 200, Ellipse(100, 60)), Location(800, 600, Rectangle(200, 100))))
  testScale("simple group times 3", simpleGroup, 3, Group(Location(600, 300, Ellipse(150, 60)), Location(1200, 900, Rectangle(300, 150))))
  testScale("complex group times 2", complexGroup, 2, Location(100, 200, Group(Ellipse(40, 80), Location(300, 100, Group(Rectangle(100, 60), Rectangle(600, 120), Location(200, 400, Ellipse(100, 60)))), Rectangle(200, 400))))

  //Size
  testSize("simple ellipse size", simpleEllipse, 1)
  testSize("simple rectangle size", simpleRectangle, 1)
  testSize("simple location size", simpleLocation, 1)
  testSize("basic group size", basicGroup, 2)
  testSize("simple group size", simpleGroup, 2)
  testSize("complex group size", complexGroup, 5)

  //Height
  testHeight("simple ellipse height", simpleEllipse, 1)
  testHeight("simple rectangle height", simpleRectangle, 1)
  testHeight("simple location height", simpleLocation, 1)
  testHeight("basic group height", basicGroup, 2)
  testHeight("simple group height", simpleGroup, 2)
  testHeight("complex group height", complexGroup, 5)
}
