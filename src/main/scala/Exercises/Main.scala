package Exercises

import cats.{Monoid, Semigroup}
import cats.instances.int._
import cats.syntax.semigroup._


object Main extends App {
  SemigroupExe1
  MonoidExe
}

/*
  Consider a function that merges two Maps that combines values if they share the same key.
  It is straightforward to write these for Maps with values of type say,
  Int or List[String], but we can write it once and for all for any type with a Semigroup instance.
*/
object SemigroupExe1 {
  def combineOptAndValue[A: Semigroup](op: Option[A], v: A) = op.map(_ |+| v).getOrElse(v)

  def mergeMap[K, V: Semigroup](lhs: Map[K, V], rhs: Map[K, V]): Map[K, V] = {
    lhs.foldLeft(rhs)({
      case (acc, (k, v)) => acc.updated(k, combineOptAndValue(rhs.get(k), v))
    })
  }

  val res = mergeMap(Map("a" -> 1), Map("a" -> 4))
  println(res)
}


/*
In the Semigroup section we had trouble writing a generic combineAll function
because we had nothing to give if the list was empty. With Monoid we can return empty,
 giving us


 */

object MonoidExe {
  def combineAll[A: Monoid](xs: List[A]) = xs.foldLeft(Monoid[A].empty)(_ |+| _)

  val res =  combineAll(List(1,2,3))
  println(res)

}