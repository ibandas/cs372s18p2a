package edu.luc.cs.laufer.cs372.shapes

object TestFixtures {

  //Ellipse Test Case
  val simpleEllipse = Ellipse(50, 30)

  //Rectangle Test Case
  val simpleRectangle = Rectangle(80, 120)

  //Location Test Case
  val simpleLocation = Location(70, 30, Rectangle(80, 120))

  //Basic Group Test Case
  val basicGroup = Group(Ellipse(50, 30), Rectangle(20, 40))

  //Simple Group Test Case
  val simpleGroup = Group(Location(200, 100, Ellipse(50, 30)), Location(400, 300, Rectangle(100, 50)))

  //Complex Group Test Case
  val complexGroup =
    Location(50, 100,
      Group(
        Ellipse(20, 40), Location(150, 50,
          Group(Rectangle(50, 30), Rectangle(300, 60),
            Location(100, 200, Ellipse(50, 30)))), Rectangle(100, 200)
      ))
}
