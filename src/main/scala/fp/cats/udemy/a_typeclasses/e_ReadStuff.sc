import java.io.FileOutputStream
import scala.util.{Try, Using}

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
object ByteEncoder {
  def apply[A](implicit ev: ByteEncoder[A]): ByteEncoder[A] = ev

  def instance[A](f: A => Array[Byte]): ByteEncoder[A] = (a: A) => f(a)

  implicit val stringByteEncoder: ByteEncoder[String] = instance[String](_.getBytes)
}

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}
object ByteDecoder {
  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev

  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] =
    (bytes: Array[Byte]) => f(bytes)

  implicit val StringByteDecoder: ByteDecoder[String] =
    instance[String](b => Try(new String(b)).toOption)
}

trait Channel {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
  def read[A](bytes: Array[Byte])(implicit dec: ByteDecoder[A]): Unit
}

object ReadAndWrite extends Channel {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(
      new FileOutputStream("C:/Users/Leonardo/Desktop/Learning/learning-cats/Channel.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }

  override def read[A](bytes: Array[Byte])(implicit dec: ByteDecoder[A]): Unit = {
    val bytesToString = dec.decode(bytes)
    print(
      bytesToString.get)
  }
}

ReadAndWrite.read(Array(98, 105, 101, 110, 32, 58, 41))
