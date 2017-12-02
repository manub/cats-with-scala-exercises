import cats._
import cats.implicits._

object Chapter10 {

  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(value: A): Either[E, A] = func(value)

    def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), _)         => e.asLeft
          case (_, Left(e))         => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }

      }
  }

}
