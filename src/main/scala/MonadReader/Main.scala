package MonadReader

import cats.data.Reader

case class Cat(name: String, favoriteFood: String)

case class Db(
               userName: Map[Int, String],
               passwords: Map[String, String]
             )

object Main extends App {

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  val greetCat: Reader[Cat, String] = catName.map(name => s"Hello $name")

  val feedCat: Reader[Cat, String] = Reader(cat => s"Have a bowl of ${cat.favoriteFood}")

  println(catName.run(Cat("Maw", "Rice")))
  println(greetCat.flatMap(greet => feedCat.map(feed => s"$greet $feed")).run(Cat("Maw", "Rice")))

  val greetAndFeed = for {
    greet <- greetCat
    feed <- feedCat
  } yield s"$greet $feed"

  println(greetAndFeed.run(Cat("Maw", "Rice")))


  ////////////////////////

  import cats.syntax.applicative._

  type DbReader[A] = Reader[Db, A]

  def findUserName(id: Int): DbReader[Option[String]] = Reader(db => db.userName.get(id))

  def checkPassword(userName: String, password: String): DbReader[Boolean] = Reader(db => db.passwords.get(userName).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      userName <- findUserName(userId)
      passwordValid <- userName.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield passwordValid


  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  println(checkLogin(1,"zerocool").run(db))
}
