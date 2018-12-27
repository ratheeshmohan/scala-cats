package MonadTransformer

import cats.data.OptionT
import scala.concurrent.Future
import cats.data.{EitherT, OptionT}

object Main extends App {

  import cats.instances.list._ // for Monad

  type ListOption[A] = OptionT[List, A]

  val result1 = OptionT(List(Option(10), Option(3)))
  val result2 = OptionT(List(Option(12), Option(11)))

  val r = result1.flatMap(x => result2.map(y => x + y))
  println(r)

  // println(List(Option(10), Option(10)))


  type ErrorOr[T] = Either[String, T]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  import cats.syntax.applicative._ // for pure
  import cats.instances.either._ // for Monad

  val a = 10.pure[ErrorOrOption]
  val b = 20.pure[ErrorOrOption]

  val c = for {
    x <- a
    y <- a
  } yield x + y

  println(c)

  //
  //For example, let’s create a Future of an Either of Option.
  /*  case class EitherT[F[_], E, A](stack: F[Either[E, A]]) {

    }*/

  type FutureEither[A] = EitherT[Future, String, A]

  type FutureEitherOption[A] = OptionT[FutureEither, A]


  import cats.instances.future._ // for Monad
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b


  val res = Await.result(futureEitherOr.value.value, 1.second)


  //5.3.5 Usage Patterns
  //Single super stack

  sealed abstract class HttpError

  final case class NotFound(item: String) extends HttpError

  final case class BadRequest(msg: String) extends HttpError

  type FutureEither1[A] = EitherT[Future, HttpError, A]


  //Unpack at consumers
  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None => Writer(List(s"Failed on $str"), None)
    }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    val result = for {
      x <- OptionT(parseNumber(a))
      y <- OptionT(parseNumber(b))
      z <- OptionT(parseNumber(c))
    } yield x + y + z

    result.value
  }

  // Exercise: Monads: Transform and Roll Out

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  //type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] = powerLevels.get(autobot) match {
    case Some(avg) => EitherT.right(Future(avg))
    case None => EitherT.left(Future("not found"))
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    for {
      x <- getPowerLevel(ally1)
      y <- getPowerLevel(ally2)
    } yield x + y > 15
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 1.second) match {
      case Right(true) => "Yes"
      case Right(false) => "No"
      case _ => "Error"
    }
  }

}
