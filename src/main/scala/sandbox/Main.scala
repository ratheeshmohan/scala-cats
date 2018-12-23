package sandbox

import java.util.Date

//import cats.syntax.show
/////////////////////

sealed trait Json

final case class JsObject(get: Map[String, Json]) extends Json

final case class JsString(get: String) extends Json

final case class JsNumber(get: Double) extends Json

final case object JNull extends Json

trait JsonWriter[-A] {
  def write(value: A): Json
}

object JsonWriterInstances {
  implicit val DoubleJsonWrite = new JsonWriter[Double] {
    def write(value: Double): Json = JsNumber(value)
  }

}

object Json {
  def write[A](a: A)(implicit p: JsonWriter[A]) = p.write(a)
}

object JsonSyntax {

  //Extension method
  implicit class JsonWriterOps[A](a: A) {
    def toJson(implicit p: JsonWriter[A]): Json = p.write(a)
  }

  implicit def JsonWriterOps[A](implicit p: JsonWriter[A]) = new JsonWriter[Option[A]] {
    def write(value: Option[A]): Json = value match {
      case Some(a) => p.write(a)
      case _ => JNull
    }
  }
}

/////////////////////////////

final case class Box[A](value: A)

trait Printable[A] {
  self =>
  def format(a: A): String

  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    def format(b: B): String = self.format(func(b))
  }

}

object PrintableInstances {

  implicit val PrintableInt = new Printable[Int] {
    def format(a: Int): String = a.toString
  }

  implicit val PrintableString = new Printable[String] {
    def format(a: String): String = a
  }

  implicit val PrintableDate = new Printable[Date] {
    def format(a: Date): String = a.toString
  }

  /*implicit def PrintableBox[A](implicit p :Printable[A]) = new Printable[Box[A]] {
    def format(box: Box[A]): String = p.format(box.value)
  }*/

  implicit def PrintableBox[A](implicit p: Printable[A]) = {
    p.contramap[Box[A]](b => b.value)
  }

}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]) = p.format(a)

  def Print[A](a: A)(implicit p: Printable[A]) = println(p.format(a))
}

final case class Cat(name: String, age: Int, color: String)

object Cat {

  import PrintableInstances._

  implicit val catPrintable = new Printable[Cat] {
    def format(c: Cat): String = s"${Printable.format(c.name)} is a ${Printable.format(c.age)} year-old ${Printable.format(c.color)} cat."
  }
}

//Extension  method
object PrintableSyntax {

  implicit class PrintableOps[A](a: A) {
    def format(implicit p: Printable[A]) = Printable.format(a)

    def Print(implicit p: Printable[A]) = println(Printable.format(a))
  }

}

////////////////////////////

object Main extends App {

  import JsonWriterInstances._
  import Cat._
  import JsonSyntax._

  println(Json.write(Option(2.4)))
  println(2.5.toJson)

  import PrintableSyntax._
  import PrintableInstances._


  //def func[A: JsonWriter](a: A)(implicit p: JsonWriter[A]) = p.write(a)

  //val c = Json.write(2.5)
  val a = Json.write(Some(2.5))
  //.toJson
  val c = Option(2.5)


  //println(Printable.format(Cat("Miky", 33, "red")))
  val c2 = Cat("Miky", 33, "red").format
  val d = new Date().format

  println(a)

  import cats.implicits._

  val shownInt = c2.show

  println(Box("hello world").format)

  (implicitly[Printable[Box[String]]]).format(Box("hello world"))
  //println(Show[Int].show(3))
}
