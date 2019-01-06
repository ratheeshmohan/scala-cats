package CaseStudy3
/*
import cats.Semigroup
import cats.syntax.monoid._
import cats.syntax.either._
import cats.data.{AndThen, Validated}
import cats.data.Validated.{Invalid, Valid}

// Trait impl.
final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(value: A): Either[E, A] = func(value)

  def and(that: CheckF[E, A])(implicit ex: Semigroup[E]): CheckF[E, A] = CheckF {
    a =>
      (this (a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (Left(e), _) => e.asLeft
        case (_, Left(e)) => e.asLeft
        case _ => a.asRight
      }
  }
}

//ADT impl.
/*
sealed trait Check[E, A] {
  def and(that: Check[E, A]) = And(this, that)

  import cats.syntax.apply._ // for mapN
  def apply(value: A)(implicit ex: Semigroup[E]): Validated[E, A] = this match {
    case Pure(f) => f(value)
    case And(c1, c2) => (c1(value), c2(value)).mapN((_, _) => value)
    case Or(c1, c2) => c1(value) match {
      case Valid(a) => Valid(a)
      case Invalid(e1) => c2(value) match {
        case Valid(a) => Valid(a)
        case Invalid(e2) => Invalid(e1 |+| e2)
      }
    }
  }
}

final case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]

final case class And[E, A](a: Check[E, A], b: Check[E, A]) extends Check[E, A]

final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
*/


//Predicates

sealed trait Predicate[E, A] {

  import Predicate._
  import cats.syntax.apply._ // for mapN

  def and(that: Predicate[E, A]) = And(this, that)

  def apply(value: A)(implicit ex: Semigroup[E]): Validated[E, A] = this match {
    case Predicate.Pure(f) => f(value)

    case And(c1, c2) => (c1(value), c2(value)).mapN((_, _) => value)

    case Or(c1, c2) => c1(value) match {
      case Valid(a) => Valid(a)
      case Invalid(e1) => c2(value) match {
        case Valid(a) => Valid(a)
        case Invalid(e2) => Invalid(e1 |+| e2)
      }
    }
  }
}

object Predicate {

  import cats.syntax.validated._

  final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

  final case class And[E, A](a: Predicate[E, A], b: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if (fn(a)) a.valid else err.invalid)

}

sealed trait Check[E, A, B] {
  import Check._

  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] =
    Map[E, A, B, C](this, f)

  def flatMap[C](f: B => Check[E, A, C]) =
    FlatMap[E, A, B, C](this, f)

  def andThen[C](next: Check[E, B, C]): Check[E, A, C] =
    AndThen[E, A, B, C](this, next)
}

object Check {
  final case class Map[E, A, B, C](
                                    check: Check[E, A, B],
                                    func: B => C) extends Check[E, A, C] {

    def apply(a: A)
             (implicit s: Semigroup[E]): Validated[E, C] =
      check(a) map func
  }

  final case class FlatMap[E, A, B, C](
                                        check: Check[E, A, B],
                                        func: B => Check[E, A, C]) extends Check[E, A, C] {

    def apply(a: A)
             (implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => func(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](
                                        check: Check[E, A, B],
                                        next: Check[E, B, C]) extends Check[E, A, C] {

    def apply(a: A)
             (implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => next(b).toEither))
  }

  final case class Pure[E, A, B](
                                  func: A => Validated[E, B]) extends Check[E, A, B] {

    def apply(a: A)
             (implicit s: Semigroup[E]): Validated[E, B] =
      func(a)
  }

  final case class PurePredicate[E, A](
                                        pred: Predicate[E, A]) extends Check[E, A, A] {

    def apply(a: A)
             (implicit s: Semigroup[E]): Validated[E, A] =
      pred(a)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] =
    PurePredicate(pred)

  def apply[E, A, B]
  (func: A => Validated[E, B]): Check[E, A, B] =
    Pure(func)
}

object Main extends App {

  import cats.instances.list._ // for Semigroup

  val c1 = CheckF[List[String], Int] { v =>
    if (v < 5) v.asRight else List("v must be < than 5").asLeft
  }
  val c2 = CheckF[List[String], Int] { v =>
    if (v > 3) v.asRight else List("v must be > than 3").asLeft
  }
  val c = c1 and c2
  println(c(2))

  // import cats.syntax.validated._

  /*
    val a: Check[List[String], Int] =
      Pure { v =>
        if (v > 2) v.valid
        else List("Must be > 2").invalid
      }

    val b: Check[List[String], Int] =
      Pure { v =>
        if (v == -2) v.valid
        else List("Must be < -2").invalid
      }

    val check: Check[List[String], Int] =
      a and b
    println(check(11))
  */


  import cats.data.{NonEmptyList}

  type Errors = NonEmptyList[String]


  def error(s: String): NonEmptyList[String] =
    NonEmptyList(s, Nil)

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


  println(Check(longerThan(3)).map(_.toString)("777"))

  val checkUsername: Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

  println(checkUsername("d"))


  ////
  val splitEmail: Check[Errors, String, (String, String)] =
    Check(_.split('@') match {
      case Array(name, domain) =>
        (name, domain).validNel[String]

      case other =>
        "Must contain a single @ character".
          invalidNel[(String, String)]
    })

  val checkLeft: Check[Errors, String, String] =
    Check(longerThan(0))

  val checkRight: Check[Errors, String, String] =
    Check(longerThan(3) and contains('.'))

  val joinEmail: Check[Errors, (String, String), String] =
    Check { case (l, r) =>
      (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkEmail: Check[Errors, String, String] =
    splitEmail andThen joinEmail
}
*/