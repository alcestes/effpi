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

object PingPong {
  implicit val timeout: Duration = Duration.Inf

  final case class Ping(replyTo: ActorRef[PongMessage])
  final case class Stop()

  sealed trait PongMessage
  object PongMessage {
    case object Pong extends PongMessage
  }

  type PongProcess = Rec[RecAt, Read[(Ping | Stop), ((SendTo[ActorRef[PongMessage], PongMessage] >>: Loop[RecAt]) | PNil)]]

  def pong(endTimePromise: Promise[Long]) = Behavior[(Ping | Stop), PongProcess] {
    prec(RecA) {
      read {
        case Ping(replyTo) =>
          // println("...pong")
          send(replyTo, PongMessage.Pong) >>
            ploop(RecA)
        case Stop() =>
          // println("...finished")
          endTimePromise.success(System.nanoTime())
          nil
      }
    }
  }

  type PingProcess[R <: ActorRef[(Ping | Stop)]] = Rec[RecAt, ((SendTo[R, Ping] >>: Read[PongMessage, Loop[RecAt]]) | SendTo[R, Stop]) ]

  def ping(max: Int, pongRef: ActorRef[(Ping | Stop)])(startTimePromise: Promise[Long]) = Behavior[PongMessage, PingProcess[pongRef.type]] {
    var count = max
    startTimePromise.success(System.nanoTime())
    prec(RecA) {
      // println("ping...")
      if (count > 0) {
        send(pongRef, Ping(self)) >>
        read { (x: PongMessage) => x match {
            case PongMessage.Pong =>
              count = count - 1
              // println(s"count = $count")
              // println("finishing...")
              ploop(RecA)
          }}
      } else {
        send(pongRef, Stop())
      }
    }
  }

  def bench(params: (Int, Int), psC: () => ProcessSystem) = {

    val (numPairs, pingTimes) = params
    implicit val ps = psC()

    val (startTimePromises, startTimeFutures): (List[Promise[Long]], List[Future[Long]]) = (1 to numPairs).toList.map { _ =>
      val startTimePromise = Promise[Long]()
      val startTimeFuture = startTimePromise.future

      (startTimePromise, startTimeFuture)
    }.unzip

    val (endTimePromises, endTimeFutures): (List[Promise[Long]], List[Future[Long]]) = (1 to numPairs).toList.map { _ =>
      val endTimePromise = Promise[Long]()
      val endTimeFuture = endTimePromise.future

      (endTimePromise, endTimeFuture)
    }.unzip


    val pongs = endTimePromises.map { endTimePromise =>
      Actor.spawn(PingPong.pong(endTimePromise))
    }

    val promisesAndPongs = startTimePromises.zip(pongs)
    promisesAndPongs.foreach{ (startTimePromise, pongRef) =>
      Actor.spawn(PingPong.ping(pingTimes, pongRef)(startTimePromise))
    }

    val startTimes = Await.result(Future.sequence(startTimeFutures), Duration.Inf)
    val startTime = startTimes.min

    val endTimes = Await.result(Future.sequence(endTimeFutures), Duration.Inf)
    val endTime = endTimes.max

    val pingPongDuration = endTime - startTime
    ps.kill()
    pingPongDuration
  }

}
