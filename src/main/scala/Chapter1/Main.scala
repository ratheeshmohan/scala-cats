package Chapter1

sealed trait Json

final case class JObject(value: Map[String, Json]) extends Json

final case class JString(value: String) extends Json

final case class JNumber(value: Double) extends Json

final case object JNull extends Json

//////////////////////////////////////


trait JsonWriter[T] {
  def write(value: T): Json
}

//////////////////////////////////////

final case class Person(name: String, email: String)

object JsonWriterInstances {
  implicit val PersonJsonWriter = new JsonWriter[Person] {
    def write(value: Person): Json = JObject(Map("name" -> JString(value.name), "email" -> JString(value.email)))
  }

  implicit def OptionWriter[A]
  (implicit writer: JsonWriter[A]) = new JsonWriter[Option[A]] {
    def write(value: Option[A]): Json = value match {
      case Some(a) => writer.write(a)
      case _ => JNull
    }
  }
}

//////////////////////////////////////
//Type class interface are function we expose to user,
//type class instance will be an implicit parameter

//Place function in a singleton object
//Interface Objects
object Json {
  def write[T](value: T)(implicit writer: JsonWriter[T]) = writer.write(value)
}

//Interface Syntax

//We can alternaঞvely use extension methods to extend exisঞng types with interface
//methods². Cats refers to this as “syntax” for the type class:
object JsonSyntax {

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]) = writer.write(value)
  }

}


object Main extends App {

  import JsonWriterInstances._
  import JsonSyntax._

  val p = Person("Ratheesh", "ram@")
  /*
  val j = implicitly[JsonWriter[Person]].write(p)
  Console.println(j)
  */
  Console.println(Json.write(p))
 // Console.println(Json.write(Option(p)))

  Console.println(p.toJson)


  //import java.awt.Color

  trait Pet[A <: Pet[A]] { this: A =>
    def name: String
    def renamed(newName: String): A
  }

  case class Fish(name: String, age: Int) extends Pet[Fish] {
    def renamed(newName: String) = copy(name = newName)
  }

  case class Kitty(name: String, color: java.awt.Color) extends Pet[Kitty] {
    def renamed(newName: String) = copy(name = newName)
  }

  def esquire[A <: Pet[A]](a: A): A = a.renamed(a.name + ", Esq.")

  val bob  = Fish("Bob", 12)
  val thor = Kitty("Thor", java.awt.Color.ORANGE)

  print(List[A forSome { type A <: Pet[A] }](bob, thor).map(esquire(_)))


}
