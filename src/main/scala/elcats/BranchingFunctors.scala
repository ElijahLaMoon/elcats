package elcats

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object FunctorSyntax {
  implicit class FunctorOps[F[_], A](src: F[A]) {
    def map[B](func: A => B)(implicit functor: Functor[F]): F[B] =
      functor.map(src)(func)
  }
}

object BranchingFunctorsMain extends App {
  import FunctorSyntax._

  implicit val treeFunctor: Functor[Tree] = 
    new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Branch(left, right) => Branch(left map f, right map f)
        case Leaf(value) => Leaf(f(value))
      }
    }

  val intTree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
  println(s"before mapping:\t${intTree}")
  println(s"after mapping:\t${intTree.map(_ * 2)}")
}
