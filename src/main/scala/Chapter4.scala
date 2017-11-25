import scala.language.higherKinds
import cats._
import cats.data.Writer
import cats.implicits._

object Chapter4 {

  object Exercise412 {

    trait Monad[F[_]] {
      def pure[A](value: A): F[A]

      def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

      def map[A, B](value: F[A])(func: A => B): F[B] =
        flatMap(value)(a => pure(func(a)))
    }

  }

  object Exercise431 {

    import Exercise412._

    class IdMonad extends Monad[Id] {
      override def pure[A](value: A): Id[A] = value

      override def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] =
        func(value)
    }

  }

  object Exercise465 {
    def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = as match {
      case head :: tail =>
        f(head, foldRight(tail, acc)(f))
      case Nil =>
        acc
    }

    def foldRight2[A, B](as: List[A], acc: B)(f: (A, B) => B): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(foldRight2(tail, acc)(f).map { acc => f(head, acc) })
        case Nil =>
          Eval.now(acc)
      }
  }

  object Exercise473 {

    type Logged[A] = Writer[Vector[String], A]

    def slowly[A](body: => A): A =
      try body finally Thread.sleep(100); Thread.`yield`()

    def factorial(n: Int): Logged[Int] =
      for {
        ans <- slowly { if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n) }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans

    def runFutures(): Unit = {
      import scala.concurrent._
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._

      val x = Await.result(Future.sequence(Vector(
        Future(factorial(3)),
        Future(factorial(3))
      )), 5.seconds)

      x.foreach(println)
    }
  }

}