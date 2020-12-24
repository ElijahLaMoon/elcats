package elcats

import cats.data.State
import State._

object PostOrderCalculatorMain extends App {
  type CalcState[A] = State[List[Int], A]

  private def isNumber(s: String): Boolean = 
    s.toIntOption.nonEmpty

  def evalOne(sym: String): CalcState[Int] = State[List[Int], Int] { state => 
    if (isNumber(sym))
      (sym.toInt :: state, sym.toInt)
    else 
      state match {
        // this part could be rewritten in terms of a higher order helper-function,
        // but I'm too lazy for doing it right now
        case n1 :: n2 :: tail if sym == "*" => (n1 * n2 :: tail, n1 * n2)
        case n1 :: n2 :: tail if sym == "+" => (n1 + n2 :: tail, n1 + n2)
    }
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    import cats.syntax.applicative._

    input.foldLeft(0.pure[CalcState]) { (acc, curr) => 
      acc.flatMap(_ => evalOne(curr))
    }
  }

  val multistagedProgram = evalAll(List("1", "2", "+", "3", "*"))
  println(multistagedProgram.runA(Nil).value)
}
