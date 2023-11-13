import cats._
import cats.implicits._

sealed trait Validated[+A]
object Validated {
  case class Valid[+A](a: A) extends Validated[A]
  case class Invalid(errors: List[String]) extends Validated[Nothing]

  implicit val applicative: Applicative[Validated] = new Applicative[Validated] {
    override def pure[A](x: A): Validated[A] = Valid(x)

    override def ap[A, B](vf: Validated[A => B])(va: Validated[A]): Validated[B] = {
      (vf, va) match {
        case (Valid(f), Valid(a)) => Valid(f(a))
        case (Invalid(f), Valid(_)) => Invalid(f)
        case (Valid(_), Invalid(a)) => Invalid(a)
        case (Invalid(f), Invalid(a)) => Invalid(f ++ a)
      }
    }

    override def map2[A, B, C](va: Validated[A], vb: Validated[B])
                              (f: (A, B) => C): Validated[C] = {

      ap( ap(pure(f.curried))(va) )  (vb)
    }

    def tupled[A, B](va: Validated[A], vb: Validated[B]): Validated[(A, B)] = {
      map2(va, vb)((a, b) => (a, b))
    }
  }
}

val v1: Validated[Int] = Applicative[Validated].pure(1)
val v2: Validated[Int] = Applicative[Validated].pure(2)
val v3: Validated[Int] = Applicative[Validated].pure(3)

(v1, v2, v3).mapN((a, b, c) =>  a + b + c)
(v1, v2).mapN((a, b) => a + b)

val optionApplicative: Applicative[Option] = new Applicative[Option] {
  override def pure[A](x: A): Option[A] = Some(x)

  override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = {
    (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _ => None
    }
  }

}

optionApplicative.map2(Some(3), Some(4))(_ + _)
optionApplicative.map2[Int, Int, Int](None, Some(4))(_ + _)

val listApplicative: Applicative[List] = new Applicative[List] {
  override def pure[A](x: A): List[A] = List(x)

  override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = {
    (ff, fa) match {
      case (f :: fs,  a :: as) =>  (a :: as).fmap(f) ++ ap(fs)(a :: as)
      case _ => Nil
    }
  }
}

listApplicative.map2(List(1, 2, 3), List(4, 5))(_ + _)
listApplicative.map2[Int, Int, Int](Nil, List(4, 5))(_ + _)