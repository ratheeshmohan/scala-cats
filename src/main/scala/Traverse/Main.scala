package Traverse

import cats.Applicative
import cats.data.Validated
import cats.syntax.all._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Main extends App {

  import scala.language.higherKinds
  import cats.syntax.apply._ // for tupled and mapN

  import cats.instances.future._
  import scala.concurrent.ExecutionContext.Implicits.global

  def listTraverse[F[_] : Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F])({ (acc, item) => (acc, func(item)).mapN(_ :+ _) })


  def int2Fut = (a: Int) => Future(a)

  val xs = (1 :: 2 :: Nil)
  val res = listTraverse(xs)(int2Fut)
  println(res)


  def listSequence[F[_] : Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val res1 = (listSequence(xs.map(int2Fut)))
  Await.ready(res1, 3.second)
  println(res1)


  // import cats.instances.vector._ // for Applicative

  // println(listSequence(List(Vector(1, 2), Vector(3, 4))))

  import cats.instances.all._

  type ErrorsOr[A] = Validated[List[String], A]

  val e = List.empty[List[Int]].pure[ErrorsOr]

  //val f = (e, Validated.invalid(List(s" is not even"))).mapN(_ :+ _)

  //println(f)

}
