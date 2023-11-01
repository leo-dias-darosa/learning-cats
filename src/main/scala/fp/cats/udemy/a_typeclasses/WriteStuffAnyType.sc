import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait WriteStuff {
  def write(obj: Any): Unit
}

object WriteFile extends WriteStuff {
  override def write(obj: Any): Unit = {
    val bytes: Array[Byte] = obj match {
      case i: Int =>
        val byteBuffer = ByteBuffer.allocate(4)
        byteBuffer.putInt(i)
        byteBuffer.array()

      case s: String => s.getBytes

      case _ => throw new Exception("unhandled")
    }

    Using(new FileOutputStream("C:/Users/Leonardo/Desktop/Learning/learning-cats/testingWriteFileAnyType")) { os =>
      os.write(bytes)
      os.flush()
    }
  }
}

WriteFile.write("Hello Any obj")