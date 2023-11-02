import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}
object ByteEncoder {
  implicit object StringByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] = s.getBytes
  }
}

trait WriteStuff {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}

object WriteObject extends WriteStuff {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(
      new FileOutputStream("C:/Users/Leonardo/Desktop/Learning/learning-cats/testingWriteFileTypeclass.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }
}

object Rotate3ByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] =
    s.getBytes.map(b => (b + 3).toByte)
}

WriteObject.write("testing")(Rotate3ByteEncoder)