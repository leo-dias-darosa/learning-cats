import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
object ByteEncoder {
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val byteBuffer = ByteBuffer.allocate(4)
    byteBuffer.putInt(n)
    byteBuffer.array()
  }
}
//
//implicit object OptionStringByteEncoder extends ByteEncoder[Option[String]] {
//  override def encode(optionString: Option[String]): Array[Byte] = optionString match {
//    case None => Array[Byte]()
//    case Some(s: String) => StringByteEncoder.encode(s)
//  }
//}
//
//implicit object OptionIntByteEncoder extends ByteEncoder[Option[Int]] {
//  override def encode(optionInt: Option[Int]): Array[Byte] = optionInt match {
//    case None => Array[Byte]()
//    case Some(s: Int) => IntByteEncoder.encode(s)
//  }
//}

implicit def optionEncoder[A](implicit encA: ByteEncoder[A]): ByteEncoder[Option[A]] = {
  new ByteEncoder[Option[A]] {
    override def encode(optionA: Option[A]): Array[Byte] = optionA match {
          case None => Array[Byte]()
          case Some(value) => encA.encode(value)
    }
  }
}

ByteEncoder[String].encode("Hello")
ByteEncoder[Int].encode(1000)

ByteEncoder[Option[String]].encode(Option("world"))
ByteEncoder[Option[String]].encode(None)

ByteEncoder[Option[Int]].encode(Option(1000))
ByteEncoder[Option[Int]].encode(None)
