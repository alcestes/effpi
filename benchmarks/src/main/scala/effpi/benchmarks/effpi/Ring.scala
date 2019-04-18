// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.effpi

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{ Future, Promise, Await }
import scala.concurrent.ExecutionContext.Implicits.global

import effpi.actor.ActorRef
import effpi.actor.dsl._
import effpi.process._
import effpi.process.dsl.{Yielding, pyield, Loop, Rec, rec => prec, loop => ploop}
import effpi.system._

object Ring {

  implicit val timeout: Duration = Duration.Inf

  enum Message {
    case Pass(msg: String, count: Int, endTimePromise: Promise[Long])
    case Stop(left: Int)
  }

  type RingInitialiser = PNil >>: Rec[RecAt, (SendTo[ActorRef[Message], Message] >>: Loop[RecAt] | RingMember)]
  // SendTo[ActorRef[Message], Message] >>: RingMember

  def ringInit
    (forwardTo: Future[ActorRef[Message]], msg: String, count: Int, members: Int, numMsgs: Int)
    (startTimePromise: Promise[Long], endTimePromise: Promise[Long]) = Behavior[Message, RingInitialiser] {
    nil >> {
      val ft = Await.result(forwardTo, Duration.Inf)
      startTimePromise.success(System.nanoTime())
      var currMsgs = 0
      prec(RecB) {
        if (currMsgs < numMsgs) {
          currMsgs += 1
          send(ft, Message.Pass(msg, count - 1, endTimePromise)) >>
            ploop(RecB)
        } else {
          ringMember(members, numMsgs, ft)()
        }
      }
    }
  }

  type RingMember = Rec[RecAt, Read[Message, ((SendTo[ActorRef[Message], Message] >>: (Loop[RecAt] |  PNil)) | Loop[RecAt] | PNil)]]

  def ringMember(members: Int, numMsgs: Int, forwardTo: ActorRef[Message]) = Behavior[Message, RingMember] {
    var msgFinished = 0
    var msgDone = 0
    prec(RecA) {
      read {
        case Message.Pass(msg, count, endTimePromise) =>
          if (count > 0) {
            send(forwardTo, Message.Pass(msg, count - 1, endTimePromise)) >>
              ploop(RecA)
          } else {
            msgFinished += 1
            send(forwardTo, Message.Stop(members - 1)) >> {
              if (msgFinished == numMsgs) {
                endTimePromise.success(System.nanoTime())
                nil
              } else {
                ploop(RecA)
              }
            }
          }
        case Message.Stop(left) =>
          msgDone += 1
          if (left > 0) {
            send(forwardTo, Message.Stop(left - 1)) >> {
              if ( msgDone == numMsgs) {
                nil
              } else {
                ploop(RecA)
              }
            }
          } else {
            if (msgDone == numMsgs) {
              nil
            } else {
              ploop(RecA)
            }
          }
      }
    }
  }

  def bench(params: (Int, Int, Int), psC: () => ProcessSystem) = {
    val (numRingMembers, numHops, numMsgs) = params
    implicit val ps = psC()
    val startTimePromise = Promise[Long]()
    val startTimeFuture = startTimePromise.future

    val endTimePromise = Promise[Long]()
    val endTimeFuture = endTimePromise.future

    val lastRingMemberPromise = Promise[ActorRef[Message]]()
    val lastRingMemberFuture = lastRingMemberPromise.future

    val ringInitialiser = Actor.spawn(
      ringInit(lastRingMemberFuture,"hello world!", numHops, numRingMembers, numMsgs)
      (startTimePromise, endTimePromise))

    var ring = new ListBuffer[ActorRef[Message]]()

    for (i <- 0 until numRingMembers) {
      if (i == 0) {
        ring += ringInitialiser
      } else {
        ring += Actor.spawn(ringMember(numRingMembers, numMsgs, ring(i - 1)))
      }
    }

    lastRingMemberPromise.success(ring.last)

    val startTime = Await.result(startTimeFuture, Duration.Inf)
    val endTime = Await.result(endTimeFuture, Duration.Inf)
    val threadRingDuration = endTime - startTime
    ps.kill()
    threadRingDuration
  }
}

