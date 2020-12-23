package elcats

import cats.Id

object MonadicSecretIdentitiesMain extends App {
  def pure[A](value: A): Id[A] = value
  def map[A, B](fa: Id[A])(f: A => B): Id[B] = 
    f(fa)
  def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] =
    f(fa)
}
