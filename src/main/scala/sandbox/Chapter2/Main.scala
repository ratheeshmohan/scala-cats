package sandbox.Chapter2



final case class Cat(name: String, age: Int, color: String)

object Cat {

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._ // for show

  implicit val catShow = new Show[Cat] {
    def show(cat: Cat): String = {
      val name = cat.name.show
      val age = cat.age.show
      val color = cat.color.show
      s"$name is a $age year-old $color cat."
    }
  }

  /* implicit val catShow = Show.show[Cat1] { cat =>
     val name  = cat.name.show
     val age   = cat.age.show
     val color = cat.color.show
     s"$name is a $age year-old $color cat."
   }*/
}

object Chapter2 extends App {

  // import cats.Show
  // import cats.instances.int._    // for Show
  import cats.syntax.show._

  // val showInt = Show.apply[Int]
  println(Cat("Garfield", 38, "ginger and black").show)

  // println(showInt.show(3))

}

