package elcats

import cats.MonadError
import cats.implicits._

object AbstractingMain extends App {
  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
    if (age >= 18) age.pure[F]
    else new IllegalArgumentException("Age is less than 18").raiseError[F, Int]

  val tryValidAge = validateAdult[scala.util.Try](18)
  val tryInvalidAge = validateAdult[scala.util.Try](8)

  type ExceptionOr[A] = Either[Throwable, A]
  val eitherInvalidAge = validateAdult[ExceptionOr](-1)

  println(tryValidAge)
  println(tryInvalidAge)
  println(eitherInvalidAge)
}
