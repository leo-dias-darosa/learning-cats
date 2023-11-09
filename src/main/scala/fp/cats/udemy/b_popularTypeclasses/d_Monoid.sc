import cats._
import cats.implicits._

case class Speed(metersPerSecond: Double) {
  def kilometersPerSec: Double = metersPerSecond / 1000
  def milesPerSec: Double = metersPerSecond / 1609.34
}

object Speed {
  def addSpeeds(speed1: Speed, speed2: Speed): Speed = {
    Speed(speed1.metersPerSecond + speed2.metersPerSecond)
  }

  implicit val eqSpeed: Eq[Speed] = Eq.fromUniversalEquals
  implicit val monoidSpeed: Monoid[Speed] = Monoid.instance(Speed(0), addSpeeds)
}

Monoid[Speed].combine(Speed(1000), Speed(3000))
Monoid[Speed].empty

Speed(1000) |+| Speed(3000)

Monoid[Speed].combineAll(List(Speed(1000), Speed(3000), Speed(5000)))
List(Speed(1000), Speed(3000), Speed(5000)).combineAll

Monoid[Speed].isEmpty(Speed(1000))
Monoid[Speed].isEmpty(Speed(0))

val sumMonoid: Monoid[Int] = Monoid.instance(0, _ + _)
val minMonoid: Monoid[Int] = Monoid.instance(Int.MaxValue, _ min _)
def listMonoid[A]: Monoid[List[A]] = Monoid.instance(List.empty[A], _ ++ _)
val stringMonoid: Monoid[String] = Monoid.instance("", _ + _)

sumMonoid.combine(2, 3)
minMonoid.combine(50, 6)
listMonoid.combine(List(1, 2, 3), List(4, 5, 6))
stringMonoid.combine("Hello ", "World!")

