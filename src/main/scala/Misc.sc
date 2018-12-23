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

List[A forSome { type A <: Pet[A] }](bob, thor).map(esquire)