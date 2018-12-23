package MonadWriter

import cats.data.Writer
import cats.syntax.applicative._
import cats.syntax.writer._
import cats.instances.vector._

import scala.concurrent.{Await, Future}

import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration._

object Main extends App {

  val w = Writer(Vector("Hi", "Kl"), 192)
  val (l, r) = w.run
  val written = w.written
  val value = w.value

  type Logged[A] = Writer[Vector[String], A]
  123.pure[Logged]


  Vector("Hi", "Kl").tell

  val x = 123.writer(Vector("Hi", "Kl")).map(_ * 3)
  println(x.run)


  def slowly[A](body: => A) = try {
    body
  } finally Thread.sleep(100)

  /*  def factorial(n: Int): Int = {
     val ans = if (n == 0) 1 else n * factorial(n - 1)
     println(s"fact $n $ans")
     ans
   }*/

  def factorial(n: Int): Logged[Int] = {
    for {
      ans <- if (n == 0) 1.pure[Logged] else slowly(factorial(n - 1).map(x => n * x))
      _ <- Vector(s"fact $n $ans").tell
    } yield ans
  }


  val Vector((logA, ansA), (logB, ansB) )= Await.result(Future.sequence(Vector(
    Future(factorial(4).run),
    Future(factorial(5).run)
  )), 5.seconds)

  print(".")
}
