// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.test.actor

object Tests {
  import effpi.actor.ActorRef
  import effpi.actor.dsl._
  import effpi.process._
  import scala.concurrent.duration.Duration

  implicit val timeout: Duration = Duration(30, "seconds")

  type Beh1 = Forever[Read[Int, Read[Int, PNil]]]

  val beh1 = Behavior[Int, Beh1] {
    forever {
      read { x =>
        read { y =>
          println(s"Received: ${x}, ${y}")
          nil
        }
      }
    }
  }

  type Beh2[R <: ActorRef[Int]] = SendTo[R, 42] >>: SendTo[R, 43]

  def beh2(ref: ActorRef[Int]) = Behavior[Nothing, Beh2[ref.type]] {
    println("Starting.  Sending 42...")
    send(ref, 42) >> {
      println("...done.  Now sending 43")
      send(ref, 43)
    }
  }

  def test1() = {
    val ps = effpi.system.ProcessSystemRunnerImproved()
    //TODO: not using ps.kill this may not terminate
    val a1 = Actor.spawn(beh1)(ps)
    val a2 = beh2(a1)
    Actor.spawn(a2)(ps)
  }
}

object Main {
  import Tests._

  def main(args: Array[String]) = {
    println("Running tests...")
    val tests = List(() => test1())
    for { t <- tests } t()
  }
}
