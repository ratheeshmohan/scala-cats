package Monoid


/*
Semigroup is just combine part of a monoid.
Often Some datatype wont have empty. Such  datatype form part of semigroup

 */


trait SemiGroup[T] {
  def combine(a: T, b: T): T
}

//If we define a monoid, we get a free semigroup.
trait Monoid[T] extends SemiGroup[T] {
  def empty: T
}


/*
Formally, a monoid for a type A is:

an operation combine with type (A, A) => A
an element empty of type A
e.g:= List & Empty
Num & 0
 */
/*
trait Monoid[T] {
  def combine(a: T, b: T): T

  def empty: T
}
*/
object Monoid {
  def AssociativeLaw[T](a: T, b: T, c: T)(implicit m: Monoid[T]) = {
    m.combine(m.combine(a, b), c) == m.combine(c, m.combine(a, b))
  }

  def IdentityLaw[T](a: T)(implicit m: Monoid[T]) = {
    m.combine(a, m.empty) == m.combine(m.empty, a) == a
  }

  //Int addition
  implicit def intAdditionMonoid : Monoid[Int] = new Monoid[Int] {
    def empty: Int = 0

    def combine(a: Int, b: Int): Int = a + b
  }

  //Boolean Monoids
  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty: Boolean = true

    override def combine(a: Boolean, b: Boolean): Boolean = a && b
  }


  implicit val booleanOrMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false

    override def combine(a: Boolean, b: Boolean): Boolean = a || b
  }

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) =
        (a && !b) || (!a && b)

      def empty = false
    }

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) =
        (!a || b) && (a || !b)

      def empty = true
    }


  //Set
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b

      def empty = Set.empty[A]
    }
}

object SemiGroup {

  //Set
  implicit def setIntersectMonoid[A] = new SemiGroup[Set[A]] {
    def combine(a: Set[A], b: Set[A]) = a intersect b
  }
}

object Main extends App {

  import Monoid._

  def combineAll[A](list: List[A])(implicit A: Monoid[A]): A = list.foldRight(A.empty)(A.combine)

  val xs : List[Int] = 1::2::Nil

  combineAll(xs)

  val intSetMonoid = implicitly[Monoid[Set[Int]]]

  val res = intSetMonoid.combine(Set(1, 2), Set(3, 4))
  println(res)

  import cats.Semigroup
  import cats.instances.int._
  import cats.instances.function._
  println(Semigroup[Int => Int].combine(_ + 9, _ * 10).apply(6))
}
