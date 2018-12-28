package Semigroupal

import cats.Semigroupal
import cats.instances.option._ // for Semigroupal

object Main extends App {
  Semigroupal.tuple3(Option(1), Option(2), Option(3))


  import cats.syntax.apply._ // for tupled and mapN
  val res = (Option(123), Option("abc")).tupled

  println(res)

  /*
  case class Cat(name: String, born: Int, color: String)

  val catOp = (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply)

  println(catOp)
*/

  case class Cat(
                  name: String,
                  yearOfBirth: Int,
                  favoriteFoods: List[String]
                )

  val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply
  val catToTuple: Cat => (String, Int, List[String]) = c => (c.name, c.yearOfBirth, c.favoriteFoods)

  println(catToTuple(tupleToCat("e", 2, Nil)))


  import cats.Monoid
  //import cats.instances.all._
  import cats.instances.string._
  import cats.instances.int._
  import cats.instances.list._
  import cats.syntax.apply._         // for imapN
  import cats.instances.invariant._  // for Semigroupal

  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  import cats.Semigroupal
  // import cats.instances.list._ // for Semigroupal

  // val res1 = Semigroupal[List].product(List(1, 2), List(3, 4))
  val res1 = (List(1, 2), List(3, 4)).tupled
  println(res1)


  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type AllErrorsOr[A] = Validated[List[String], A]

  val res2 = Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1","w")),
    Validated.invalid(List("Error 2")))
  println(res2)

  val res3 = (Validated.invalid(List("Error 1", "r")),Validated.invalid(List("Error 9")),
    Validated.invalid(List("Error 2"))).tupled
  println(res3)


}
