package CaseStudy1

import scala.concurrent.Future
import cats.Id
//import cats.Applicative

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int]
}

case class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Int =
    hosts.getOrElse(hostname, 0)
}


//import cats.instances.list._ // for Traverse
//import cats.syntax.foldable._
/*
class UptimeService[F[_]: Applicative]
(client: UptimeClient[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] = ???
   // hostnames.traverse(client.getUptime).map(_.sum)
}

object Main extends App {
  def testTotalUptime() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  testTotalUptime()


 import cats._
 import cats.instances.all._
 import cats.syntax.all._
 import cats.syntax.traverse._

 //List("1", "2", "3").traverse(Option.apply)

 /*
 import cats._
import cats.instances.appl._
import cats.syntax.traverse._

List("1", "2", "3").traverse(Option.apply)

}
  */

  */