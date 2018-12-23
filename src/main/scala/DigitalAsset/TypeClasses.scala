package DigitalAsset


import scala.language.higherKinds


trait Functor[F[_]] {
  def map1[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = fa => map1(fa)(f)
}

object ListInstances {
  implicit def listFunctor = new Functor[List] {
    def map1[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}

object FunctorSyntax {
  implicit class FunctorOps[F[_], A](fa: F[A]) {
    //For testing the ext method.
    def select[B](f: A => B)(implicit ev: Functor[F]) = {
      ev.map1(fa)(f)
    }
  }

}

object Main extends App {
  import ListInstances._
  import FunctorSyntax._

  val x = (1 :: 2 :: 3 :: Nil)
  println(implicitly[Functor[List]].map1(x)(_ + 2))
  println(x.select(x => x + 3))

  val f = (x: Int) => x.toString
  val lXs = implicitly[Functor[List]].lift(f)(x)

  println("..")
}