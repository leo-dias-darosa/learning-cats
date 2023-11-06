import cats._
import cats.implicits._

case class Account(
                    id: Long,
                    number: String,
                    balance: Double,
                    owner: String
                  )
object Account {
  implicit def orderById(implicit orderLong: Order[Long]): Order[Account] =
    Order.from((a1, a2) => orderLong.compare(a1.id, a2.id))

  object Instances {
    implicit val orderByNumber: Order[Account] =
      Order.by(_.number)

    implicit val orderByBalance: Order[Account] = {
      Order.by(_.balance)
    }
  }
}

def sort[A](list: List[A])(implicit orderA: Order[A]): List[A] = {
  list.sorted(orderA.toOrdering)
}

val account1 = Account(4, "222-22", 1000.0, "leo")
val account2 = Account(2, "333-33", 1001.0, "amanda")
val account3 = Account(1, "444-44", 1002.0, "guiba")
val account4 = Account(3, "555-55", 1003.0, "izzie")

sort[Account](List(account1, account2, account3, account4))

//import Account.Instances.orderByBalance
sort[Account](List(account1, account2, account3, account4))

account1 compare account2
account1 max account2
account4 min account1

implicit val orderByIdDesc: Order[Account] = Order.reverse(Account.orderById)
sort[Account](List(account1, account2, account3, account4))