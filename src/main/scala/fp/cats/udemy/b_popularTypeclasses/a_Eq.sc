import cats._
import cats.implicits._

case class Account(
                    id: Long,
                    number: String,
                    balance: Double,
                    owner: String
                  )
object Account {
  implicit val universalEq: Eq[Account] = Eq.fromUniversalEquals

  object Instances {
    implicit def byIdEq(implicit eqLong: Eq[Long]): Eq[Account] =
      Eq.instance[Account]((a1, a2) => eqLong.eqv(a1.id, a2.id))

    implicit def byIdEq2: Eq[Account] =
      Eq.by(_.id)

    implicit def byNumberEq: Eq[Account] =
      Eq.by(_.number)
  }
}

val accountLeo = Account(1, "101", 1000.0, "leo")
val accountAmanda = Account(2, "101", 1001.0, "amanda")

Eq[Account]
  .eqv(
    accountLeo,
    accountAmanda
  )

Account.Instances.byIdEq.eqv(accountLeo, accountAmanda)
Account.Instances.byNumberEq.eqv(accountLeo, accountAmanda)

accountLeo === accountAmanda

implicit val eqToUse: Eq[Account] = Account.Instances.byNumberEq
accountLeo === accountAmanda
