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

val optionFunctor: Functor[Option] = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
    case None => None
    case Some(a) => Some(f(a))
  }
}

val listFunctor: Functor[List] = new Functor[List] {
  override def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
    case Nil => Nil
    case hd :: tl => f(hd) :: map(tl)(f)
  }
}

optionFunctor.map(Some(3))(_ + 1)
listFunctor.map(List(1,2,3,4))(_ + 2)