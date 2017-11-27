import cats._
import cats.data._
import cats.implicits._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Chapter5 {

  object Exercise54 {

    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )

    def getPowerLevel(autobot: String): Response[Int] =
      powerLevels.get(autobot) match {
        case Some(level) => EitherT.right(Future.successful(level))
        case None        => EitherT.left(Future.successful(s"$autobot unreachable"))
      }

    def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
      level1 <- getPowerLevel(ally1)
      level2 <- getPowerLevel(ally2)
    } yield (level1 + level2) > 15

    def tacticalReport(ally1: String, ally2: String): String = {
      val stack = canSpecialMove(ally1, ally2).value

      Await.result(stack, 1.second) match {
        case Left(err) => s"Comms error: $err"
        case Right(false) => s"$ally1 and $ally2 need a recharge."
        case _ => s"$ally1 and $ally2 are ready to roll out!"
      }
    }

  }
}
