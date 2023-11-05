case class Person(name: String, id: Int)

object Person {
  object Instances {
    // TODO #9: Define an Eq instance for Person comparing them by name
    //          Extra points: receive an implicit instance for String and use it
    implicit def personNameEq(implicit eqString: Eq[String]): Eq[Person] =
      Eq.instance((a, b) => eqString.eq(a.name, b.name))

    // TODO #10: Define an Eq instance for Person comparing them by id
    //           Extra points: receive an implicit instance for Int and use it
    implicit def personIdEq(implicit eqId: Eq[Int]): Eq[Person] =
          Eq.instance((a, b) => eqId.eq(a.id, b.id))
  }
}

trait Eq[A] {
  // TODO #1: Define an 'eq' method that takes two A values as parameters, and returns a Boolean
  def eq(a: A, b: A): Boolean
}
object Eq {
  // TODO #2: Define the method 'apply' so we can summon instances from implicit scope
  def apply[A](implicit eqTrait: Eq[A]): Eq[A] = eqTrait

  // TODO #3: Define the method 'instance' so we can build instances of the Eq typeclass more easily.
  //          It should take as the only parameter a function of type (A, A) => Boolean
  def instance[A](f: (A, A) => Boolean): Eq[A] = (a: A, b: A) => f(a, b)

  // TODO #4: Define an Eq instance for String
  implicit val stringEq: Eq[String] = instance[String]((a, b) => a == b)

  // TODO #5: Define an Eq instance for Int
  implicit val intEq: Eq[Int] = instance[Int]((a, b) => a == b)

  // TODO #6: Define an Eq instance for Person. Two persons are equal if both their names and ids are equal.
  //          Extra points: receive implicit instances for String and Int and use them
  implicit def personEq(eqString: Eq[String], eqInt: Eq[Int]): Eq[Person] = instance[Person](
    (a, b) =>
      eqString.eq(a.name, b.name) && eqInt.eq(a.id, b.id)
  )

  // TODO #7: Provide a way to automatically derive instances for Eq[Option[A]] given that we have an implicit
  //          instance for Eq[A]
  implicit def optionEq[A](implicit eqA: Eq[A]): Eq[Option[A]] = {
    (a: Option[A], b: Option[A]) => (a, b) match {
      case (None, _) => false
      case (_, None) => false
      case (None, None) => true
      case (Some(value1), Some(value2)) => eqA.eq(value1, value2)
    }
  }
}

// TODO #8: Define a class 'EqOps' with a method 'eqTo' that enables the following syntax:
//          "hello".eqTo("world")
implicit class EqOps[A](a: A) {
  def eqTo(b: A)(implicit eqA: Eq[A]): Boolean = {
    eqA.eq(a, b)
  }
}

"hello".eqTo("world")
"hello".eqTo("hello")