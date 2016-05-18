package funpats

import java.nio.ByteBuffer
import scala.util.Try
import scodec.bits.ByteVector

/**
  * Created by walter
  */
case class Address(host: String, port: Int)

trait Codec[A] {
  def encode(x: A, bv: ByteVector): Option[ByteVector]
  def decode(bv: ByteVector): Option[(A, ByteVector)]
}

object Codec {
  def apply[A : Codec]: Codec[A] = implicitly[Codec[A]]
  implicit val intCodec = new Codec[Int] {
    def encode(x: Int, bv: ByteVector) =
      Try(ByteBuffer.allocate(4).putInt(x).array).toOption.map(bv ++ ByteVector(_))
    def decode(bv: ByteVector) =
      Try(ByteBuffer.wrap(bv.take(4).toArray).getInt).toOption.map((_, bv.drop(4)))
  }
  implicit val stringCodec = new Codec[String] {
    def encode(x: String, bv: ByteVector) = for {
      bs <- Try(x.getBytes("utf-8")).toOption
      bv2 <- Codec[Int].encode(bs.length, bv)
    } yield bv2 ++ ByteVector(bs)
    def decode(bv: ByteVector) = for {
      (l, bv2) <- Codec[Int].decode(bv)
      s <- Try(new String(bv2.take(l).toArray, "utf-8")).toOption
    } yield (s, bv2.drop(l))
  }
  implicit val addressCodec = new Codec[Address] {
    def encode(x: Address, bv: ByteVector) = for {
      bv2 <- Codec[String].encode(x.host, bv)
      bv3 <- Codec[Int].encode(x.port, bv2)
    } yield bv3
    def decode(bv: ByteVector) = for {
      (h, bv2) <- Codec[String].decode(bv)
      (p, bv3) <- Codec[Int].decode(bv2)
    } yield (Address(h, p), bv3)
  }
  implicit class Ops[A : Codec](x: A) {
    def ~>(bv: ByteVector): Option[ByteVector] = Codec[A].encode(x, bv)
  }
}

object CodecExamples extends App {
  import Codec._
  for {
    bv <- 123 ~> ByteVector.empty
    bv2 <- "hello" ~> bv
    (i, bv3) <- Codec[Int].decode(bv2)
    (s, bv4) <- Codec[String].decode(bv3)
  } assert(i == 123 && s == "hello" && bv4 == ByteVector.empty)
  val a = Address("localhost", 1234)
  for {
    bv <- a ~> ByteVector.empty
    (a2, bv2) <- Codec[Address].decode(bv)
  } assert(a == a2 && bv2 == ByteVector.empty)
}