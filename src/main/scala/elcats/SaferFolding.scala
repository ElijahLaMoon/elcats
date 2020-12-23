package elcats

import cats.Eval

object SaferFoldingMain extends App {
  private def foldRightEval[A, B](as: List[A], acc: Eval[B])
    (fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => 
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil => 
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    (foldRightEval(as, Eval.now(acc)){ (a, b) => b.map(fn(a, _)) }).value

  println(foldRight(List(1,2,3), 1)(_ * _))
}
