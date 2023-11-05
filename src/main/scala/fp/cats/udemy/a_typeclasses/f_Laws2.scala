package fp.cats.udemy.a_typeclasses

import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.typelevel.discipline.Laws
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

import java.nio.ByteBuffer
import scala.util.Try

object testing {
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
  }

  trait ByteCodecLaws[A] {
    def codec: ByteCodec[A]
    def isomorphism(a: A): Boolean = codec.decode(codec.encode(a)).contains(a)
  }

  trait ByteCodecTests[A] extends Laws {
    def laws: ByteCodecLaws[A]

    def byteCodec(implicit arb: Arbitrary[A]): RuleSet = new DefaultRuleSet(
      name = "bytecodec",
      parent = None,
      "isomorphism" -> forAll(laws.isomorphism _)
    )
  }
  object ByteCodecTests {
    def apply[A](implicit bc: ByteCodec[A]): ByteCodecTests[A] = {
      new ByteCodecTests[A] {
        override def laws: ByteCodecLaws[A] = new ByteCodecLaws[A] {
          override def codec: ByteCodec[A] = bc
        }
      }
    }
  }

  implicit object IntByteCodec extends ByteCodec[Int] {
    override def encode(n: Int): Array[Byte] = {
      val byteBuffer: ByteBuffer = ByteBuffer.allocate(4)
      byteBuffer.putInt(n)
      byteBuffer.array()
    }

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

  implicit object StringByteCodec extends ByteCodec[String] {
    override def decode(bytes: Array[Byte]): Option[String] =
      Try(new String(bytes)).toOption

    override def encode(s: String): Array[Byte] = s.getBytes
  }

}

import testing._

class ByteCodecSpec extends AnyFunSuite with Configuration with FunSuiteDiscipline {
  checkAll("ByteCodec[Int]", ByteCodecTests[Int].byteCodec)
  checkAll("ByteCodec[String]", ByteCodecTests[String].byteCodec)
}


