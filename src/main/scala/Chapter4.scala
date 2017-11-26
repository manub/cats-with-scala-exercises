import scala.language.higherKinds
import cats._
import cats.data.{Reader, State, Writer}
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
          Eval.defer(foldRight2(tail, acc)(f).map { acc =>
            f(head, acc)
          })
        case Nil =>
          Eval.now(acc)
      }
  }

  object Exercise473 {

    type Logged[A] = Writer[Vector[String], A]

    def slowly[A](body: => A): A =
      try body
      finally Thread.sleep(100);
    Thread.`yield`()

    def factorial(n: Int): Logged[Int] =
      for {
        ans <- slowly {
          if (n == 0) 1.pure[Logged] else factorial(n - 1).map(_ * n)
        }
        _ <- Vector(s"fact $n $ans").tell
      } yield ans

    def runFutures(): Unit = {
      import scala.concurrent._
      import scala.concurrent.ExecutionContext.Implicits.global
      import scala.concurrent.duration._

      val x = Await.result(Future.sequence(
                             Vector(
                               Future(factorial(3)),
                               Future(factorial(3))
                             )),
                           5.seconds)

      x.foreach(println)
    }
  }

  object Exercise483 {

    type DbReader[A] = Reader[Db, A]

    case class Db(
        usernames: Map[Int, String],
        passwords: Map[String, String]
    )

    def findUsername(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    def checkLogin(userId: Int, password: String): DbReader[Boolean] =
      //      sol 1
      //      findUsername(userId).flatMap { usernameOpt =>
      //        usernameOpt.map { username =>
      //          checkPassword(username, password)
      //        }.getOrElse(Reader(_ => false))
      //      }
      for {
        username <- findUsername(userId)
        passwordOk <- username
          .map { username =>
            checkPassword(username, password)
          }
          .getOrElse(false.pure[DbReader])
      } yield passwordOk

  }

  object Exercise493 {

    import State._

    type CalcState[A] = State[List[Int], A]

    def evalOne(sym: String): CalcState[Int] = {
      def operand(operand: String): CalcState[Int] = State[List[Int], Int] { state =>
        val operands = state.take(2)
        val result = operand match {
          case "+" => operands.head + operands.last
          case "-" => operands.head - operands.last
          case "*" => operands.head * operands.last
          case "/" => operands.head / operands.last
        }
        (result +: state.drop(2), result)
      }

      sym match {
        case "+" | "-" | "*" | "/" => operand(sym)
        case num => State[List[Int], Int] { state => (num.toInt +: state, num.toInt) }
      }
    }

    def evalAll(input: List[String]): CalcState[Int] =
      input.foldLeft(0.pure[CalcState]) { (state, sym) =>
        state.flatMap(_ => evalOne(sym))
      }

    def evalInput(expr: String, stack: List[Int]): Int =
      evalAll(expr.toList.map(_.toString)).runA(stack).value

  }

  object Exercise4101 {

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
    def leaf[A](value: A): Tree[A] = Leaf(value)


    val treeMonad = new Monad[Tree] {

      override def pure[A](x: A): Tree[A] = leaf(x)

      override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
        case Leaf(a) => f(a)
        case Branch(l, r) => branch(flatMap(l)(f), flatMap(r)(f))
      }

      override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
        f(a) match {
          case Leaf(Right(b)) => pure(b)
          case Leaf(Left(a1)) => tailRecM(a1)(f)
          case Branch(l, r) => Branch(
            flatMap(l) {
              case Left(l1) => tailRecM(l1)(f)
              case Right(l1) => pure(l1)
            },
            flatMap(r) {
              case Left(r1) => tailRecM(r1)(f)
              case Right(r1) => pure(r1)
            }
          )

        }
    }

  }
}
