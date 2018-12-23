package MonadState

import cats.Monad
import cats.data.State


sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]


object Main extends App {
  /*
    val a  = State[Int,String]{state => (state + 1, s"Prev state was $state")}
    println(a.run(3).value)
    println(a.runS(3).value)
    println(a.runA(3).value)
    */

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1 : $ans")
  }


  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2 : $ans")
  }


  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)
  println(both.run(5).value)


  val pureDemo = State.pure[Int, String]("Result")
  println(pureDemo.run(33).value)

  ////////////////////////////////////
  //4.9.3 Exercise: Post-Order Calculator

  type CalcState[A] = State[List[Int], A]


  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }


  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case x :: y :: tail =>
        val ans = func(x, y)
        (ans :: tail, ans)

      case _ => sys.error("fail")
    }

  //println(evalOne("42").run(Nil).value)

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(State.pure[List[Int], Int](0)) {
      (acc, b) => acc.flatMap(_ => evalOne(b))
    }


  def evalInput(input: String): Int =
    evalAll(input.split(" ").toList).runA(Nil).value


  val program = evalAll(List("1", "2", "+", "3", "*"))
  println(program.run(Nil).value)

  println(evalInput("1 2 + 3 4 + *"))
  ///////////////

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)


  implicit val treeMonad = new Monad[Tree] {
    def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = tree match {
      case Leaf(a) => f(a)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???

    def pure[A](x: A): Tree[A] = leaf(x)
  }
}
