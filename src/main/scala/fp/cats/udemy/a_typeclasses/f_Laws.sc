import scala.util.Try

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

// Enconding and decoding (or vice-versa) should return the original value
trait ByteCodec[A]
  extends ByteEncoder[A]
    with ByteDecoder[A] {
  def isomorphic(a: A)(implicit codec: ByteCodec[A]): Boolean
}
object ByteCodec {
  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev

  def instance[A](f: Array[Byte] => Option[A], g: A => Array[Byte]): ByteCodec[A] = new ByteCodec[A] {

    override def isomorphic(a: A)(implicit codec: ByteCodec[A]): Boolean =
      decode(encode(a)).contains(a)

    override def encode(a: A): Array[Byte] = g(a)

    override def decode(bytes: Array[Byte]): Option[A] = f(bytes)
  }

  implicit val stringByteCodec: ByteCodec[String] =
    instance[String](
      b => Try(new String(b)).toOption,
      _.getBytes
    )
}

ByteCodec.stringByteCodec.isomorphic("Hello")

