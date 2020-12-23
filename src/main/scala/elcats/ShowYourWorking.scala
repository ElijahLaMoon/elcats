package elcats

import cats.data._
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._

object ShowYourWorkingMain extends App {
  type Logged[A] = Writer[Vector[String], A]

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] = {
    val ans = slowly {
      if (n == 0) 1.pure[Logged]
      else factorial(n - 1).mapBoth { (log, computedFactorial) =>
          (log, n * computedFactorial)
      }
    }
    
    ans.mapWritten(_ :+ s"fact of $n: ${ans.value}")
  }

  val concurrentFactorials = Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(8))
  )), 5.seconds)
  
  concurrentFactorials.map(_.written).foreach(println)
}
