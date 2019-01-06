package FormValidation

import FormValidation.Check.AndThen
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.apply._
import cats.syntax.validated._
import cats.syntax.semigroup._ // for |+|

trait Predicate[E, A] {

  import Predicate._

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


sealed trait Check[E, A, B] {

  import Check._

  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]) = FlatMap[E, A, B, C](this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
}

object Check {

  final case class Pure[E, A, B](f: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = f(a)
  }

  final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(a)
  }

  case class Map[E, A, B, C](check: Check[E, A, B],
                             f: B => C) extends Check[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check(a) map (f)

  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B],
                                       f: B => Check[E, A, C]) extends Check[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](check: Check[E, A, B],
                                       next: Check[E, B, C]) extends Check[E, A, C] {

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => next(b).toEither))
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)

  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] = Pure(func)

}


object Main extends App {

  import cats.data.{NonEmptyList, Validated}
  import cats.syntax.apply._ // for mapN
  import cats.syntax.validated._ // for valid and invalid

  type Errors = NonEmptyList[String]

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

  val checkUsername: Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.

  val splitEmail: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) =>
        (name, domain).validNel[String]

      case other =>
        "Must contain a single @ character".
          invalidNel[(String, String)]
    })


  val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
  val checkRight: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))

  val joinEmail: Check[Errors, (String, String), String] =
    Check {
      case (l, r) =>
        (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }
  val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail


  final case class User(username: String, email: String)

  def createUser(username: String,
                 email: String): Validated[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)


  println(createUser("Ratheesh", "r@bb.o"))
  println(createUser("", "@"))

}
