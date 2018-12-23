package variances


trait Ex {
  def map[A, B]: (A => B) => (() => A) => () => B
}

object Main extends App {

}

