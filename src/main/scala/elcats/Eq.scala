package elcats

import cats.Eq
import cats.syntax.eq._

object EqualityLibertyFelinityMain extends App {
  import PrintableMain.Cat

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  implicit val catEq: Eq[Cat] = 
    Eq.instance[Cat] { (c1, c2) => {
      import cats.instances.int._
      import cats.instances.string._

      c1.name === c2.name &&
      c1.age === c2.age &&
      c1.color === c2.color
    }}

  println(s"cat1 === cat2: ${cat1 === cat2}")
  println(s"cat1 =!= cat2: ${cat1 =!= cat2}")

  println(s"optionCat1 === optionCat2: ${optionCat1 === optionCat2}")
  println(s"optionCat1 =!= optionCat2: ${optionCat1 =!= optionCat2}")
}
