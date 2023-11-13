import cats._
import cats.implicits._

object MonadEither {
  implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {

    def pure[A](a: A): Either[E, A] = Right(a)

    def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
      fa match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }
    }

    def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???
  }
}

5.asRight[String].flatMap(x => (x + 1).asRight[String])
5.asRight[String].flatMap(x => "boom".asLeft[Int].flatMap(y => "boom2".asLeft[Int]))