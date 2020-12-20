package elcats

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = 
    monoid
}

object MonoidLaws {
  def assosiativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = 
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  
  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = 
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
}

object MonoidsTruthMain extends App {
  import MonoidLaws._

  private val booleans = List(true, false)
  val testCases = 
    for {
      e1 <- booleans
      e2 <- booleans
      e3 <- booleans
    } yield (e1, e2, e3)

  private def lines = println("-------------------------------------------------")

  {
    implicit val boolOrMonoid = 
      new Monoid[Boolean] {
        def combine(x: Boolean, y: Boolean): Boolean = x || y
        def empty: Boolean = false
      }
    
    lines
    println(s"assosiativity law holds for all boolean OR cases: ${testCases.map(x => assosiativeLaw(x._1, x._2, x._3)).forall(_ == true)}")
    println(s"boolean OR, identity law holds for true value: ${identityLaw(true)}")
    println(s"boolean OR, identity law holds for false value: ${identityLaw(false)}")
  }

  {
    implicit val boolAndMonoid =
      new Monoid[Boolean] {
        def combine(x: Boolean, y: Boolean): Boolean = x && y
        def empty: Boolean = true
      }
    
    lines
    println(s"assosiativity law holds for all boolean AND cases: ${testCases.map(x => assosiativeLaw(x._1, x._2, x._3)).forall(_ == true)}")
    println(s"boolean AND, identity law holds for true value: ${identityLaw(true)}")
    println(s"boolean AND, identity law holds for false value: ${identityLaw(false)}")
  }

  {
    implicit val boolXorMonoid = 
      new Monoid[Boolean] {
        def combine(x: Boolean, y: Boolean): Boolean = (x || y) && (!x || !y)
        def empty: Boolean = false
      }
    
    lines
    println(s"assosiativity law holds for all boolean XOR cases: ${testCases.map(x => assosiativeLaw(x._1, x._2, x._3)).forall(_ == true)}")
    println(s"boolean XOR, identity law holds for true value: ${identityLaw(true)}")
    println(s"boolean XOR, identity law holds for false value: ${identityLaw(false)}")
  }

  {
    implicit val boolXnorMonoid = 
      new Monoid[Boolean] {
        def combine(x: Boolean, y: Boolean): Boolean = (x && y) || (!x && !y)
        def empty: Boolean = true
      }
    
    lines
    println(s"assosiativity law holds for all boolean XNOR cases: ${testCases.map(x => assosiativeLaw(x._1, x._2, x._3)).forall(_ == true)}")
    println(s"boolean XNOR, identity law holds for true value: ${identityLaw(true)}")
    println(s"boolean XNOR, identity law holds for false value: ${identityLaw(false)}")
  }

  lines
}
