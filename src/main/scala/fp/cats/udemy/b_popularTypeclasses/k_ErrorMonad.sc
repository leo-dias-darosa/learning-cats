import java.io.IOException
import scala.util.{Failure, Success, Try}
import cats._
import cats.implicits._

trait HttpMethod
case object GET extends HttpMethod
case class HttpRequest(method: HttpMethod, url: String)
case class HttpResponse(statusCode: Int)

def doRequest(request: HttpRequest): HttpResponse = {
  if(math.random() < 0.5) throw new IOException("BOOM!")
  else HttpResponse(200)
}

def executeRequest(req: HttpRequest): Option[HttpResponse] = {
  try {
    Some(doRequest(req))
  } catch {
    case _: IOException => None
  }
}

def executeRequest2(req: HttpRequest): Either[String, HttpResponse] = {
  try {
    Right(doRequest(req))
  } catch {
    case _: IOException => Left("Sorry")
  }
}

def executeRequest3(req: HttpRequest): Try[HttpResponse] = {
  try {
    Success(doRequest(req))
  } catch {
    case e: IOException => Failure(e)
  }
}

executeRequest(HttpRequest(GET, "www.example.com"))
executeRequest2(HttpRequest(GET, "www.example.com"))
executeRequest3(HttpRequest(GET, "www.example.com"))

val optionME: MonadError[Option, Unit] = new MonadError[Option, Unit] {
  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
  override def raiseError[A](e: Unit): Option[A] = None
  override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] =
    fa.orElse(f(()))

  override def pure[A](x: A): Option[A] = Some(x)
}

object MonadErrorInstances {
  def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???

    override def raiseError[A](e: E): Either[E, A] = Left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] = {
      fa match {
        case Right(a) => Right(a)
        case Left(e) => f(e)
      }
    }

    override def pure[A](x: A): Either[E, A] = Right(x)
  }


  val tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = ???

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

    override def raiseError[A](e: Throwable): Try[A] = Failure(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = {
      fa match {
        case Success(a) => Success(a)
        case Failure(e) => f(e)
      }
    }

    override def pure[A](x: A): Try[A] = ???
  }
}