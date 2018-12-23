package Functors

import cats.Functor

//import scala.io.Source


/*
Functor Laws

Functors guarantee the same semantics whether we sequence many small operations one by one, or combine them into a larger function before mapping. To ensure this is the case the following laws must hold:

Identity: calling map with the identity function is the same as doing nothing:

fa.map(a => a) == fa
Composition: mapping with two functions f and g is the same as mapping with f and then mapping with g:

fa.map(g(f(_))) == fa.map(f).map(g)

 */


object Math {

  import cats.syntax.functor._ // for map

  def doMath[F[_]](start: F[Int])
                  (implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)
}
/*

trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](a: A) extends Tree[A]

*/
sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]



object Tree {
  import cats.Functor

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
        tree match {
          case Branch(left, right) =>
            Branch(map(left)(func), map(right)(func))
          case Leaf(value) =>
            Leaf(func(value))
        }
    }

/*

  implicit val TreeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(l, r) => {
        val l1 = map(l)(f)
        val r1 = map(r)(f)
        Branch(l1, r1)
      }
      case Leaf(a) => Leaf(f(a))
    }
  }*/


}

object Main extends App {

  // import cats.instances.list._
  import cats.instances.option._
  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map

  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2

  val func3 =
    ()=> (y: Double) => y * 2

  val res = (func1 map func2) (5)
  println(res) // composition using map

  val negate = (x: Int) => -x

  val l = 1 :: 2 :: Nil
  // val r = Functor[List].map(l)(-_)
  val liftedF = Functor[Option].lift(negate)
  val f = liftedF(Some(3))
  println(f)


  println(Math.doMath(Option(6)))


  Tree.branch(Leaf(10), Leaf(20)).map(a=>a*3)


}
