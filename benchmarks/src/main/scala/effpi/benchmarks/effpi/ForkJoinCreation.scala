// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.effpi

import scala.concurrent.duration.Duration
import scala.concurrent.{ Future, Promise, Await }
import scala.concurrent.ExecutionContext.Implicits.global

import effpi.actor.ActorRef
import effpi.actor.dsl._
import effpi.process._
import effpi.process.dsl.{Yielding, pyield, Loop, Rec, rec => prec, loop => ploop}
import effpi.system._

object ForkJoinCreation {

  implicit val timeout: Duration = Duration.Inf

  case class Message(msg: String)

  type SimpleActor = Read[Message, PNil]

  val simpleActor = Behavior[Message, SimpleActor] {
    read { _ =>
      nil
    }
  }

  type Sender = Rec[RecAt, (SendTo[ActorRef[Message], Message] >>: Loop[RecAt] | PNil)]

  def sender(receivers: Array[ActorRef[Message]])(endTimePromise: Promise[Long]) = Behavior[Nothing, Sender] {
    var i = 0
    prec(RecA) {
      if (i < receivers.length) {
        send(receivers(i), Message("Hello, World!")) >> {
          i += 1
          ploop(RecA)
        }
      } else {
        endTimePromise.success(System.nanoTime())
        nil
      }
    }
  }

  def bench(numActors: Int, psC: () => ProcessSystem) = {

    implicit val ps = psC()
    val endTimePromise = Promise[Long]()
    val endTimeFuture = endTimePromise.future

    val startTime = System.nanoTime()

    val simpleActorsRef = (1 to numActors).toArray.map{ _ => Actor.spawn(simpleActor)}
    Actor.spawn(sender(simpleActorsRef)(endTimePromise))(ps)

    val endTime = Await.result(endTimeFuture, Duration.Inf)
    val creationDuration = endTime - startTime
    ps.kill()
    creationDuration
  }

}
