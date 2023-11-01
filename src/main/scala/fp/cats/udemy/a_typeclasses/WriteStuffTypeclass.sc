import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

trait WriteStuff {
  def write[A](
              obj: A,
              enc: ByteEncoder[A]
              ): Unit
}

object WriteObject extends WriteStuff {
  override def write[A](obj: A, enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(
      new FileOutputStream("C:/Users/Leonardo/Desktop/Learning/learning-cats/testingWriteFileTypeclass.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }
}

object IntByteEncoder extends ByteEncoder[Int] {
  override def encode(n: Int): Array[Byte] = {
    val byteBuffer = ByteBuffer.allocate(4)
    byteBuffer.putInt(n)
    byteBuffer.array()
  }
}

WriteObject.write[Int](20, IntByteEncoder)

object StringByteEncoder extends ByteEncoder[String] {
  override def encode(s: String): Array[Byte] = s.getBytes
}

WriteObject.write("leo dias", StringByteEncoder)