package ContraVariant

trait Codec[A] {
  self =>

  def encode(value: A): String

  def decode(value: String): A

  def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
    def encode(value: B): String = self.encode(enc(value))

    def decode(value: String): B = dec(self.decode(value))
  }

}

case class Box[A](value: A)


object Codec {
  implicit val stringCodec = new Codec[String] {
    def encode(value: String): String = value

    def decode(value: String): String = value
  }

  implicit val doubleCodec = stringCodec.imap[Double](_.toDouble, _.toString)

  implicit def boxCodec[A](implicit ev: Codec[A]): Codec[Box[A]] =
    ev.imap(Box[A](_), _.value)

  def encode[A](a: A)(implicit ev: Codec[A]) = ev.encode(a)
  def decode[A](str: String)(implicit ev: Codec[A]) = ev.decode(str)
}

object Main extends App {
import Codec._

  val r = encode(123.4)
  // res0: String = 123.4

  val r1 = decode[Double]("123.4")
  // res1: Double = 123.4

  val r2 = encode(Box(123.4))
  // res2: String = 123.4

  val r3 = decode[Box[Double]]("123.4")
  // res3: Box[Double] = Box(123.4)

  val g =4
  Symbol
}
