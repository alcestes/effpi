// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.effpi

import scala.concurrent.duration.Duration
import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global

import effpi.actor.ActorRef
import effpi.actor.dsl._
import effpi.process._
import effpi.process.dsl.{Yielding, pyield, Loop, Rec, rec => prec, loop => ploop}
import effpi.system._

object ForkJoinThroughput {

  implicit val timeout: Duration = Duration.Inf

  case class Message(msg: String)

  sealed abstract class RecAt2[A] extends RecVar[A]("InfiniteActorLoop")
  case object RecA2 extends RecAt2[Unit]

  type SimpleActor = Rec[RecAt, (Read[Message, Loop[RecAt]] | PNil)]

  def simpleActor(maxMsgs: Int) = Behavior[Message, SimpleActor] {
    var count = 0
    prec(RecA) {
      if (count < maxMsgs) {
        read { (_: Message) =>
          count += 1
          ploop(RecA)
        }
      } else {
        nil
      }
    }
  }

  def bench(param: (Int, Int), psC: () => ProcessSystem) = {

    val (numActors, numMessages) = param
    implicit val ps = psC()
    val startTimePromise = Promise[Long]()
    val startTimeFuture = startTimePromise.future

    val endTimePromise = Promise[Long]()
    val endTimeFuture = endTimePromise.future

    val simpleActorsRef = (1 to numActors).map{ _ => Actor.spawn(simpleActor(numMessages))}

    val startTime = System.nanoTime()
    (1 to numMessages).foreach { n =>
      // println(n)
      simpleActorsRef.foreach { simpleActor =>
        simpleActor.send(Message("Hello World!"))
      }
    }
    val endTime = System.nanoTime()

    val throughputDuration = endTime - startTime
    ps.kill()
    throughputDuration
  }

}
