package elcats

import cats._

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]) = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]) = println(p.format(value))
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

object PrintableInstances {
  implicit val printableString: Printable[String] = 
    new Printable[String] {
      def format(value: String): String = value
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
      import PrintableInstances._

      def format(value: Cat): String = {
        val name = Printable.format(value.name)
        val age = Printable.format(value.age)
        val color = Printable.format(value.color)
        s"$name is a $age year-old $color cat."
      }
    }

    val cat = Cat("Murchyk", 8, "grey")
    print("cat printed with interface object: ")
    Printable.print(cat)

    print("cat printed with interface syntax: ")
    import PrintableSyntax._
    cat.print
}
