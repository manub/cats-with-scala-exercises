import cats._
import cats.implicits._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Chapter9 {

  def foldMap[B: Monoid, A](a: Vector[A])(f: A => B): B =
    a.map(f).combineAll

  def parallelFoldMap[A, B: Monoid](values: Vector[A])(
      func: A => B): Future[B] = {
    val processors = Runtime.getRuntime.availableProcessors()
    val numberOfChunks = (values.length / processors).ceil.toInt
    val chunks = values.grouped(numberOfChunks).toVector

    Future
      .sequence(chunks.map(chunk =>
        Future {
          foldMap(chunk)(func)
      }))
      .map(_.combineAll)
  }

  def parallelFoldMapCats[A, B: Monoid](values: Vector[A])(
      func: A => B): Future[B] = {

    val processors = Runtime.getRuntime.availableProcessors()
    val numberOfChunks = (values.length / processors).ceil.toInt
    val chunks = values.grouped(numberOfChunks).toVector

    Future
      .sequence(chunks.map(chunk => Future { chunk.foldMap(func) }))
      .map(_.combineAll)
  }
}
