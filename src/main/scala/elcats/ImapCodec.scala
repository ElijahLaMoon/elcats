package elcats

trait Codec[A] { self =>
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = 
    new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))
      def decode(value: String): B = dec(self.decode(value))
    }
}

object CodecInstances {
  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }
    
  implicit val intCodec: Codec[Int] = 
    stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)
}

object ImapCodecMain extends App {
  import CodecInstances._

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)
  
  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  val encodedInt = encode(123)
  val decodedInt = decode[Int](encodedInt)

  val encodedBool = encode(true)
  val decodedBool = decode[Boolean](encodedBool)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)
  val encodedDouble = encode(123.4)
  val decodedDouble = decode[Double](encodedDouble)

  import PrintableCMain.Box
  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    stringCodec.imap[Box[A]](d => Box(c.decode(d)), e => c.encode(e.value))

  println(s"${encode(Box(123.4))}")
  println(s"${decode[Box[String]]("123.4")}")
}
