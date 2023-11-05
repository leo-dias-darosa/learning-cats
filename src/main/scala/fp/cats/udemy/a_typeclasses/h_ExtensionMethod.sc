import java.nio.ByteBuffer

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

implicit object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val byteBuffer = ByteBuffer.allocate(4)
    byteBuffer.putInt(n)
    byteBuffer.array()
  }
}

implicit object StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes
}

implicit class ByteEncoderOps[A](val a: A) extends AnyVal {
  def encode(implicit enc: ByteEncoder[A]): Array[Byte] = {
    enc.encode(a)
  }
}

5.encode
"hello world".encode

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

implicit object IntByteDecoder extends ByteDecoder[Int] {
  override def decode(bytes: Array[Byte]): Option[Int] = {
    if (bytes.length != 4) None
    else {
      val byteBuffer: ByteBuffer = ByteBuffer.allocate(4)
      byteBuffer.put(bytes)
      byteBuffer.flip()
      Some(byteBuffer.getInt)
    }
  }
}

implicit class ByteDecoderOps[A](val bytes: Array[Byte]) extends AnyVal{
  def decode(implicit dec: ByteDecoder[A]): Option[A] = {
    dec.decode(bytes)
  }
}

Array[Byte](0, 0, 0, 5).decode
Array[Byte](0, 0, 0, 0, 5).decode