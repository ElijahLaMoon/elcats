package elcats

trait SuperAdder[A] {
  def add(items: List[A]): A
}

object SuperAdder {
  def apply[A](implicit superAdder: SuperAdder[A]) = 
    superAdder
}

object AddingThingsMain extends App {
  import cats._
  import syntax.monoid._
  import instances.int._

  implicit val intAdder = 
    new SuperAdder[Int] {
      def add(items: List[Int]): Int = items
        .fold(Monoid.empty[Int]){ _ |+| _ }
    }

  implicit val optionIntAdder = 
    new SuperAdder[Option[Int]] {
      def add(items: List[Option[Int]]) = items
        .fold(Monoid.empty[Option[Int]]){ _ |+| _ }
    }

  final case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid =
    new Monoid[Order] {
      def combine(x: Order, y: Order): Order = 
        Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
      def empty: Order = Order(0.0, 0.0)
    }

  implicit val orderAdder =
    new SuperAdder[Order] {
      def add(items: List[Order]): Order = items
        .fold(Monoid[Order].empty){ _ |+| _ }
    }

  val ints = List(1,2,3,4)
  val options = ints.map(Option(_))
  val orders = List(Order(5.47, 1.0), Order(4.53, 2.0), Monoid[Order].empty)

  println(s"sum of ints: ${SuperAdder[Int].add(ints)}")
  println(s"sum of options: ${SuperAdder[Option[Int]].add(options)}")
  println(s"sum of orders: ${SuperAdder[Order].add(orders)}")
}
