package Monad

trait Monad1[F[_]] {
  def pure1[A](a: A): F[A]

  def flatMap1[A, B](a: F[A])(func: A => F[B]): F[B]

  def map1[A, B](fa: F[A])(func: A => B) = flatMap1(fa)(_ => pure1(func(_)))

  // def map1[A, B](value: F[A])(func: A => B) = flatMap1(value)(a => pure1(func(a)))
}

object Monad1 {

  implicit val OptionMonad = new Monad1[Option] {
    def pure1[A](a: A) = Some(a)

    def flatMap1[A, B](value: Option[A])(func: A => Option[B]) = value match {
      case Some(a) => func(a)
      case None => None
    }
  }
}

object MonadExt {

  import cats.Id
  import cats.Monad

  implicit val IdMonad = new Monad[Id] {
    def pure[A](value: A): Id[A] = value

    def flatMap[A, B](value: Id[A])(f: A => Id[B]): Id[B] = f(value)

    def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ???
  }

  object Main extends App {
    val d = implicitly[Monad1[Option]].flatMap1(Some(4))(x => Some(x * 3))
    println(d)


    //From CATS
    import cats.Monad
    import cats.implicits._
    import cats.Eval

    println(Monad[Option].map(Some(4))(x => x * 5))

    def sumSquare[F[_]](a: F[Int], b: F[Int])(implicit m: Monad[F]) = {
      a.flatMap(x => b.map(y => x * x + y * y))
    }

    def sumSquare1[F[_]](a: F[Int], b: F[Int])(implicit m: Monad[F]) = {
      for {
        x <- a
        y <- b
      } yield x * x + y * y
    }

    println(sumSquare(Option(2), Option(4)))
    println(sumSquare1(Option(2), Option(4)))

    import cats.Id

    println(sumSquare(2: Id[Int], 4: Id[Int]))

    val a = Monad[Id].pure(3)
    println(sumSquare(a, a))



    //-----------
    def factorial(n: BigInt): Eval[BigInt] =
      if (n == 1) Eval.now(1) else Eval.defer(factorial(n - 1).map(_ * n))

    println(factorial(50000).value)

    //--------------


    def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
      as match {
        case head :: tail =>
          Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
        case Nil =>
          acc
      }

    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      foldRightEval(as, Eval.now(acc))((a, b) => b.map(fn(a, _))).value
  }

}
