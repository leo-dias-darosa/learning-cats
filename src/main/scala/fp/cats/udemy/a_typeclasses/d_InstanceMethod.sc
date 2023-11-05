trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
object ByteEncoder {
  implicit val stringByteEncoder: ByteEncoder[String] = instance[String](_.getBytes)

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = (a: A) => f(a)
}

implicit val rotateStringByteEncoder: ByteEncoder[String] =
  ByteEncoder
    .instance[String](_.getBytes.map(b => (b + 3).toByte))
