import cats.Functor

class Secret[A](val value: A) {
  private def hashed: String = value.toString.reverse

  override def toString: String = hashed
}
object Secret {
  implicit val secretFunctor: Functor[Secret] = new Functor[Secret] {
    override def map[A, B](fa: Secret[A])(f: A => B): Secret[B] = {
      new Secret(f(fa.value))
    }
  }
}

val leonardoSecret: Secret[String] = new Secret("Leonardo")
leonardoSecret.value

val upperCaseLeonardoSecret = Functor[Secret].map(leonardoSecret)(_.toUpperCase)
upperCaseLeonardoSecret.value

val optionFunctor: Functor[Option] = ???
val listFunctor: Functor[List] = ???