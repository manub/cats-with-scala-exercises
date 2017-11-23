import cats._

object Chapter3 {

  object Functors {

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    object Tree {
      def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
      def leaf[A](a: A): Tree[A] = Leaf(a)
    }

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Leaf(x) => Leaf(f(x))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
    }
  }

  final case class Box[A](value: A)

  object ContravariantFunctors {

    trait Printable[A] {
      self =>

      def format(value: A): String
      def contramap[B](func: B => A): Printable[B] = (value: B) => self.format(func(value))
    }

    def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

    implicit val stringPrintable: Printable[String] = (value: String) => "\"" + value + "\""

    implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"


    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
      (box: Box[A]) => format(box.value)
  }

  object InvariantFunctors {

    trait Codec[A] {
      self =>
      def encode(value: A): String
      def decode(value: String): A
      def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))
        override def decode(value: String): B = dec(self.decode(value))
      }
    }

    def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)
    def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

    implicit val stringCodec: Codec[String] = new Codec[String] {
      override def encode(value: String): String = value
      override def decode(value: String): String = value
    }

    implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)
    implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)
    implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

    implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = new Codec[Box[A]] {
      override def encode(box: Box[A]): String = c.encode(box.value)
      override def decode(box: String): Box[A] = Box(c.decode(box))
    }




  }
}
