package edu.luc.cs.laufer.cs372.shapes

//Used old 313 project as a reference along with Scala API Documentation

object boundingBox {
  def apply(s: Shape): Location = s match {
    case Rectangle(x, y) => Location(0, 0, s)
    case Ellipse(m, n)   => Location(0 - m, 0 - n, Rectangle(2 * m, 2 * n))
    case Group(shapes @ _*) => {
      val boxes = shapes.map(s => boundingBox(s))
      val minX = boxes.map(p => p.x).min
      val maxX = boxes.map(p => p.x + p.shape.asInstanceOf[Rectangle].width).max
      val minY = boxes.map(p => p.y).min
      val maxY = boxes.map(p => p.y + p.shape.asInstanceOf[Rectangle].height).max

      Location(minX, minY, Rectangle(maxX - minX, maxY - minY))
    }
    case Location(x, y, s) => {
      val box = boundingBox(s)
      Location(x + box.x, y + box.y, box.shape)
    }
  }

  //Scale Function
  def scale(s: Shape, factor: Int): Shape = s match {
    case Ellipse(m, n)   => Ellipse(m * factor, n * factor)
    case Rectangle(x, y) => Rectangle(x * factor, y * factor)
    case Location(x, y, s) => {
      val shape = scale(s, factor)

      Location(x * factor, y * factor, shape)
    }
    case Group(shapes @ _*) => {
      val totalGroup = shapes.map(shape => scale(shape, factor))
      Group(totalGroup: _*)
    }
  }

  //Size Function
  def size(s: Shape): Int = s match {
    case Rectangle(x, y)   => 1
    case Ellipse(m, n)     => 1
    case Location(x, y, s) => size(s)
    case Group(shapes @ _*) => {
      shapes.map(size).sum
    }

  }

  //Height Function
  def height(s: Shape): Int = s match {
    case Rectangle(x, y)   => 1
    case Ellipse(m, n)     => 1
    case Location(x, y, s) => 1 + height(s)
    case Group(shapes @ _*) => {
      1 + shapes.map(height).max
    }
  }

}
