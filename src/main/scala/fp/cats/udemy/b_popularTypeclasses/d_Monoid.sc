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