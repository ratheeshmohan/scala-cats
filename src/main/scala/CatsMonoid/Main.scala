package CatsMonoid

import cats.Semigroup
import cats.Monoid
import cats.instances.string._
import cats.syntax.semigroup._


object SuperAdder {

  def add[T](items: List[T])(implicit m: Monoid[T]) = {
    //items.foldLeft(0)(_ + _)
    items.foldLeft(m.empty)(_ |+| _)
  }
}

case class Order(totalCost: Double, quantity: Double)

object Order {
  implicit val orderMonoid = new Monoid[Order] {
    override def empty: Order = Order(0, 0)

    override def combine(x: Order, y: Order): Order = new Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }
}

final case class NonEmptyList[A](head: A, tail: List[A]) {
  def ++(other: NonEmptyList[A]): NonEmptyList[A] = NonEmptyList(head, tail ++ other.toList)

  def toList = head :: tail
}

object NonEmptyList {
  implicit def NonEmptyListSemigroup[A] = new Semigroup[NonEmptyList[A]] {
    def combine(x: NonEmptyList[A], y: NonEmptyList[A]): NonEmptyList[A] = x ++ y
  }
}

object MonoidInstances {
  implicit def OptionMonoid[A: Semigroup] = new Monoid[Option[A]] {
    def empty: Option[A] = None

    def combine(x: Option[A], y: Option[A]): Option[A] = x match {
      case None => y
      case Some(xv) => {
        y match {
          case None => x
          case Some(yv) => Some(xv |+| yv)
        }
      }
    }
  }

}


object Main extends App {
  val res = Semigroup[String].combine("a", "b")
  println(res)

  import cats.instances.int._ // for Monoid
  import cats.instances.option._ // for Monoid

  println(SuperAdder.add(1 :: 2 :: Nil))
  println(SuperAdder.add(Some(1) :: Some(2) :: None :: Nil))

  //println("a" |+| "b" |+| Monoid[String].empty)

  println(SuperAdder.add(Order(1, 2) :: Order(3, 6) :: Nil))
  println(SuperAdder1.add(Order(1, 2) :: Order(3, 6) :: Nil))


  ///////
  val xs = List(NonEmptyList(1, 2 :: 3 :: Nil), NonEmptyList(4, 5 :: 6 :: Nil))
  //SuperAdder1.add(xs) , wont compile as NonEmptyList is not a monoid
  //So lift it to Option
  val lifted = xs.map(Option(_))
  println(SuperAdder1.add(lifted))

  println(Semigroup.combineAllOption(xs))
}


object SuperAdder1 {

  def add[T](items: List[T])(implicit ev: Monoid[T]): T = {
    items.foldLeft(ev.empty)(ev.combine)
  }
}

trait Functor[F[_]]{
  def map[A, B](fu : F[A], f : A=>B) : F[B]
}