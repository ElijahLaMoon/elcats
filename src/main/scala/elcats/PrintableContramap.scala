package elcats

trait PrintableC[A] { self =>
  def format(value: A): String

  def contramap[B](func: B => A): PrintableC[B] = 
    new PrintableC[B] {
      def format(value: B): String = 
        self.format(func(value))
    }
}

object PrintableC {
  def format[A](value: A)(implicit p: PrintableC[A]) = p.format(value)
  def print[A](value: A)(implicit p: PrintableC[A]) = println(p.format(value))
}

object PrintableCSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)
    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }
}

object PrintableCInstances {
  implicit val stringPrintableC: PrintableC[String] =
    new PrintableC[String] {
      def format(value: String): String = 
        s"'${value}'"
    }

  implicit val booleanPrintableC: PrintableC[Boolean] =
    new PrintableC[Boolean] {
      def format(value: Boolean): String = 
        if(value) "yes" else "no"
    }
}

object PrintableCMain extends App {
  import PrintableC._
  import PrintableCSyntax._
  import PrintableCInstances._

  print("hello")
  print(true)

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: PrintableC[A]): PrintableC[Box[A]] =
    p.contramap[Box[A]](_.value)

  print(Box("hello world"))
  print(Box(true))
}
