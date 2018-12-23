package EitherMonad

import cats.Eval


sealed trait LoginError extends Product with Serializable

final case class UserNotFound(username: String)
  extends LoginError

final case class PasswordIncorrect(username: String)
  extends LoginError

case object UnexpectedError extends LoginError

case class User(username: String, password: String)

///////////////////////////////////////////////////////


object Main extends App {

  import cats.syntax.either._

  //4.4.2 Creating Instances - Either
  val u = 1.asRight[String]
  val v = 4.asRight[String]
  val res = for {
    x <- u
    y <- v
  } yield (x + y)

  println(res)

  println(Either.catchNonFatal("ddd".toInt))

  type LoginResult = Either[LoginError, User]

  // Choose error-handling behaviour based on type:
  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")

      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")

      case UnexpectedError =>
        println(s"Unexpected error")
    }

  User("sd", "sd").asRight.fold(handleError, println)
  UserNotFound("e").asLeft.fold(handleError, println)

  /////////////////////////////////////////////////////
  import cats.MonadError
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)

  val failure = monadError.raiseError("Badness")

  val result = monadError.handleError(failure) {
    case "Badness" => monadError.pure(4)
    case other => monadError.raiseError("not ok")
  }

  val f = cats.Monad[ErrorOr].pure(4) // for ensure

  val exn: Throwable =
    new RuntimeException("It's all gone wrong")


  def factorial(n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.later(n) else Eval.defer(factorial(n - 1).map(_ * n))

  println(factorial(50000).value)

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case x :: xs => Eval.defer(fn(x, foldRightEval(xs, acc)(fn)))
    case Nil => acc
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) {
      (a, b) => b.map(fn(a, _))
    }.value
}
