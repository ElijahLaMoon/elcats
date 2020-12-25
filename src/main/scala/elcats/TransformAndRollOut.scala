package elcats

import cats.data.EitherT
import cats.syntax.monad._
import cats.implicits._

import concurrent.{Await, Future}
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global

object TransformAndRollOutMain extends App {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(value) => value.pure[Response]
      case None => s"Error: $autobot is unreachable".raiseError[Response, Int]
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = 
    for {
      l1 <- getPowerLevel(ally1)
      l2 <- getPowerLevel(ally2)
    } yield (l1 + l2 > 15) 
  
  def tacticalReport(ally1: String, ally2: String): String = { 
    val report = canSpecialMove(ally1, ally2).value.map {
      case Left(errorLog) => errorLog
      case Right(movePossible) => 
        if (movePossible) s"$ally1 and $ally2 are ready to roll out!"
        else s"$ally1 and $ally2 need a recharge"
    }
  
    Await.result(report, 3.seconds)
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Bumblebee", "Hot Rod"))
  println(tacticalReport("Jazz", "Ironhide"))
}
