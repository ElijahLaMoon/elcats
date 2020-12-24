package elcats

import cats.Monad
import cats.syntax.flatMap._

object BranchingMonadsMain extends App {
  implicit val treeMonad: Monad[Tree] =
    new Monad[Tree] {
      def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = 
        fa match {
          case Branch(left, right) => Branch(left flatMap f, right flatMap f)
          case Leaf(value) => f(value)
        }

      // this solution isn't actually tail recursive
      def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = 
        flatMap(f(a)) {
          case Left(value) => tailRecM(value)(f)
          case Right(value) => Leaf(value)
        }

      def pure[A](x: A): Tree[A] = Leaf(x)
    }
}
