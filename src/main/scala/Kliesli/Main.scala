package Kliesli


import cats.Semigroup
import cats.data.{Kleisli, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.semigroup._ // for |+|
import cats.instances.either._ // for Semigroupal

trait Predicate[E, A] {

  import Predicate._

  def run(implicit s: Semigroup[E]): A => Either[E, A] =
    (a: A) => this (a).toEither

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(f) => f(a)

    case And(left, right) =>
      (left(a), right(a)).mapN((_, _) => a)

    case Or(left, right) =>
      left(a) match {
        case Valid(a1) => Valid(a)
        case Invalid(e1) =>
          right(a) match {
            case Valid(a2) => Valid(a)
            case Invalid(e2) => Invalid(e1 |+| e2)
          }
      }
  }

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
}

object Predicate {

  case class Pure[F, E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  case class And[F, E, A, B](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]

  case class Or[F, E, A, B](l: Predicate[E, A], r: Predicate[E, A]) extends Predicate[E, A]

  def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] = Pure(a => if (fn(a)) a.valid else err.invalid)
}


object Main extends App {

  import cats.data.{NonEmptyList}
  import cats.syntax.apply._ // for mapN

  type Errors = NonEmptyList[String]
  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]


  // Create a check from a function:
  def Check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)

  // Create a check from a Predicate:
  def CheckPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)


  def error(s: String) = NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.size > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      str => str.filter(c => c == char).size == 1)


  // A username must contain at least four characters
  // and consist entirely of alphanumeric characters

  val checkUsername: Check[String, String] =
    CheckPred(longerThan(3) and alphanumeric)

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.

  val splitEmail: Check[String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) =>
        Right((name, domain))

      case other =>
        Left(error("Must contain a single @ character"))
    })


  val checkLeft: Check[String, String] = CheckPred(longerThan(0))
  val checkRight: Check[String, String] = CheckPred(longerThan(3) and contains('.'))

  val joinEmail: Check[(String, String), String] =
    Check {
      case (l, r) =>
        (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }
  val checkEmail: Check[String, String] = splitEmail andThen joinEmail


  final case class User(username: String, email: String)

  def createUser(
                  username: String,
                  email: String): Either[Errors, User] = (
    checkUsername.run(username),
    checkEmail.run(email)
  ).mapN(User)


  println(createUser("Ratheesh", "r@bb.o"))
  println(createUser("", ""))
}
