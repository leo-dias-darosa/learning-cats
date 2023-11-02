import java.io.FileOutputStream
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

trait WriteStuff {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}

object WriteObject extends WriteStuff {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(
      new FileOutputStream("C:/Users/Leonardo/Desktop/Learning/learning-cats/SwitchTesting.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }
}

case class Switch(isOn: Boolean)
object Switch {
  implicit object SwitchByteEncoder extends ByteEncoder[Switch] {
    override def encode(switch: Switch): Array[Byte] =
      Array((if (switch.isOn) '1' else '0').toByte)
  }
}

WriteObject.write(Switch(false))