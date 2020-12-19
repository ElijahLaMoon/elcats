package elcats

import cats._

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]) = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]) = println(p.format(value))
}

object PrintableInstances {
  implicit val printableString: Printable[String] = 
    new Printable[String] {
      def format(value: String): String = value.toString
    }

  implicit val printableInt: Printable[Int] = 
    new Printable[Int] {
      def format(value: Int): String = value.toString
    }
}

object PrintableMain extends App {
  final case class Cat(name: String, age: Int, color: String)

  implicit val printableCat: Printable[Cat] = 
    new Printable[Cat] {
      def format(value: Cat): String = s"${value.name} is a ${value.age} year-old ${value.color} cat."
    }

    val cat = Cat("Murchyk", 8, "grey")
    Printable.print(cat)
}
