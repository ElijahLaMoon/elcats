package elcats

import cats._
import cats.implicits._

object ShowMain extends App {
  import PrintableMain.Cat
  val cat = Cat("Murchyk", 8, "grey")

  {
    implicit val catShow: Show[Cat] = 
      new Show[Cat] {
        def show(c: Cat): String = {
          val name = c.name.show
          val age = c.age.show
          val color = c.color.show
          s"$name is a $age year-old $color cat."
        }
      }

      print("cat printed with Cat's Show typeclass: ")
      println(cat.show)
  }
  
  {
    implicit val catShow2: Show[Cat] = 
      Show.show(c => s"${c.name} is a ${c.age} year-old ${c.color} cat.")

    print("cat printed with Cat's Show typeclass syntax: ")
    println(cat.show)
  }

}
