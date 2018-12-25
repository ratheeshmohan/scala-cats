package MonadTransformer
import cats.data.OptionT

object Main extends App {
  import cats.instances.list._     // for Monad

  type ListOption[A] = OptionT[List, A]

  val result1 = OptionT(List(Option(10),Option(3)))
  val result2 = OptionT(List(Option(12),Option(11)))

  val r = result1.flatMap(x =>  result2.map(y => x + y))
  println(r)

 // println(List(Option(10), Option(10)))
}
