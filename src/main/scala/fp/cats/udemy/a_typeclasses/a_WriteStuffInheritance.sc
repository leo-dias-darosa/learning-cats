import java.io.FileOutputStream
import scala.util.Using

trait EncodableObj {
  def encode(): Array[Byte]
}

trait WriteStuff {
  def write(obj: EncodableObj): Unit
}

case class fullName(firstName: String, lastName: String) extends EncodableObj {
  override def encode(): Array[Byte] = firstName.getBytes ++ " ".getBytes ++  lastName.getBytes
}

object WriteFile extends WriteStuff {
  override def write(obj: EncodableObj): Unit = {
    val bytes: Array[Byte] = obj.encode()

    Using(
      new FileOutputStream("C:/Users/Leonardo/Desktop/Learning/learning-cats/testingWriteFileInheritance")) {
      os =>
      os.write(bytes)
      os.flush()
    }
  }
}

WriteFile.write(fullName("leo", "dias"))