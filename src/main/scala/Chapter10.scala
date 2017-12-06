import cats._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
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

  sealed trait Check[E, A] {

    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
      case Pure(func) => func(a)

      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a) // intelliJ doesn't like this

      case Or(left, right) =>
        left(a) match {
          case Valid(_) => Valid(a)
          case Invalid(e1) =>
            right(a) match {
              case Valid(_)    => Valid(a)
              case Invalid(e2) => Invalid(e1 |+| e2)
            }
        }
    }
  }

  final case class And[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]

  final case class Or[E, A](left: Check[E, A], right: Check[E, A])
      extends Check[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

  // val a: Check[List[String], Int] = Pure { v => Validated.valid(v).ensure(List("must be > 2"))(_ > 2) }

}
