sealed trait Validated[+A]

case class Valid[+A](a: A) extends Validated[A]

case class Invalid(errors: List[String]) extends Validated[Nothing]

def validateName (name: String): Validated[String] = {
  if (name.forall(_.isLetter)) Valid(name)
  else Invalid(List("Name cannot contain numbers or special characters"))
}
def validateAge(age: Int): Validated[Int] = {
  if (age >= 18) Valid(age)
  else Invalid(List("You must be over 18"))
}

case class Person(name: String, age: Int)

def validatePerson(person: Person): Validated[Person] = {
  val validatedName = validateName(person.name)
  val validatedAge = validateAge(person.age)
  (validatedName, validatedAge) match {
    case (Valid(name), Valid(age)) => Valid(person)
    case (Invalid(nameErrors), Invalid(ageErrors)) => Invalid(nameErrors ++ ageErrors)
    case (Invalid(nameErrors), _) => Invalid(nameErrors)
    case (_, Invalid(ageErrors)) => Invalid(ageErrors)
  }
}

def map2[A, B, C](va: Validated[A], vb: Validated[B])(f: (A, B) => C): Validated[C] = {
  (va, vb) match {
    case (Valid(a), Valid(b)) => Valid(f(a, b))
    case (Invalid(aErrors), Invalid(bErrors)) => Invalid(aErrors ++ bErrors)
    case (Invalid(aErrors), _) => Invalid(aErrors)
    case (_, Invalid(bErrors)) => Invalid(bErrors)
  }


}