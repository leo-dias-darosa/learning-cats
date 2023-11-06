import cats._
import cats.implicits._

case class Account(
                    id: Long,
                    number: String,
                    balance: Double,
                    owner: String
                  )

object Account {
  implicit val toStringShow: Show[Account] = Show.fromToString

  object Instances {
    implicit val byOwnerAndBalance: Show[Account] = Show.show { account =>
      s"${account.owner} - $$${account.balance}"
    }

    implicit val accountOwnerMessage: Show[Account] = Show.show { account =>
      s"This account belongs to ${account.owner}"
    }
  }
}

val leo = Account(1, "1234", 1000.0, "Leo")
Account.toStringShow.show(leo)

Account.Instances.byOwnerAndBalance.show(leo)
Account.Instances.accountOwnerMessage.show(leo)