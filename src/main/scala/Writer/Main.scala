package Writer

import scala.concurrent.{Await, Future}


object Math {
  def slowly[A](body: => A) = try body finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val res = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $res")
    res
  }
}

object Main extends App {

  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  Await.result(Future.sequence(Vector(
    Future(Math.factorial(5)),
    Future(Math.factorial(5))
  )), 5.seconds)


  import cats.data.Writer
  import cats.implicits._

  type Logged[A] = Writer[Vector[String], A]

  42.pure[Logged]


}
