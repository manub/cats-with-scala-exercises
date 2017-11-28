import cats._
import cats.implicits._

object Chapter7 {

  object Exercise712 {

    def withLeft[A](list: List[A]): List[A] = list.foldLeft(List.empty[A])((a, i) => i :: a)

    def withRight[A](list: List[A]): List[A] = list.foldRight(List.empty[A])((i, a) => i :: a)
  }

  object Exercise713 {

    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B]) { (item, acc) => f(item) :: acc }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldRight(List.empty[B]) { (item, acc) => f(item) ::: acc }

    def filter[A](list: List[A])(f: A => Boolean): List[A] =
      list.foldRight(List.empty[A]) { (item, acc) =>
         if (f(item)) item :: acc else acc
      }

    def sum[A: Numeric](list: List[A]): A = {
      val numeric = implicitly[Numeric[A]]
      list.foldRight(numeric.zero)(numeric.plus)
    }

    def sum[A: Monoid](list: List[A]): A = {
      val monoid = implicitly[Monoid[A]]
      list.foldRight(monoid.empty)(monoid.combine)
    }
  }
}
