package CaseStudy2

import cats.Monoid
import cats.syntax.monoid._

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Main extends App {
  def foldMap[A, B: Monoid](v: Vector[A])(f: A => B): B = v.foldLeft(Monoid[B].empty)((b, a) => b |+| f(a))

  def parallelFoldMap[A, B: Monoid](v: Vector[A])(f: A => B): Future[B] = {
    val p = Runtime.getRuntime.availableProcessors
    val futures = v.grouped(p).map { group =>
      Future {

        foldMap(group)(f)

      }
    }
    Future.sequence(futures).map(xs => xs.foldLeft(Monoid[B].empty)(_ |+| _))
  }


  import cats.instances.int._ // for Monoid
  println(foldMap(Vector(1, 2, 3))(identity))
  val result = (parallelFoldMap(Vector(1, 2, 3))(identity))

  println(Await.result(result, 1.second))

}
