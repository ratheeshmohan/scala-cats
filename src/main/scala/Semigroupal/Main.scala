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
  implicit val catMonoid: Monoid[Cat] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

}
