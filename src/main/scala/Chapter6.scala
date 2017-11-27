import cats._
import cats.data._
import cats.implicits._

object Chapter6 {

  object Exercise6311 {

    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      for {
        x <- x
        y <- y
      } yield (x, y)
  }

  object Exercise644 {

    case class User(name: String, age: Int)

    type FormData = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]

    def getValue(map: FormData, field: String): FailFast[String] =
      map.get(field).toRight(List(s"missing field $field"))

    def parseInt(value: String): FailFast[Int] =
      Either
        .catchOnly[NumberFormatException](value.toInt)
        .leftMap(_ => List("invalid number"))

    def nonBlank(value: String): FailFast[String] =
      Right(value).ensure(List("empty string"))(_.nonEmpty)

    def nonNegative(value: Int): FailFast[Int] =
      Right(value).ensure(List("non negative"))(_ > 0)


    def readName(form: FormData): FailFast[String] = for {
      name <- getValue(form, "name")
      valid <- nonBlank(name)
    } yield valid

    def readAge(form: FormData): FailFast[Int] = for {
      age <- getValue(form, "age")
      ageNum <- parseInt(age)
      valid <- nonNegative(ageNum)
    } yield valid

    def readUser(form: FormData): FailSlow[User] = (
      readName(form).toValidated,
      readAge(form).toValidated
    ).mapN(User.apply)
  }

}
