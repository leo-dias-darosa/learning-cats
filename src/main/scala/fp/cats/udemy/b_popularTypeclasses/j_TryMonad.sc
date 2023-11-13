import cats._
import cats.implicits._
import scala.util._

implicit val tryMonad: Monad[Try] = new Monad[Try] {
  def pure[A](a: A): Try[A] = Success(a)

  def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = {
    fa match {
      case Success(a) => f(a)
      case Failure(e) => Failure(e)
    }
  }

  def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
}

tryMonad.pure(5)
tryMonad.pure(5).flatMap(i => tryMonad.pure(i + 1))
tryMonad.pure(5).flatMap(i => Failure(new Exception("boom")))