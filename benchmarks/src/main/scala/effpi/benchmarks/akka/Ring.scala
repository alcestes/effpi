// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.akka

import akka.NotUsed
import akka.actor.typed.scaladsl.{ Behaviors, AbstractBehavior, ActorContext}
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior, DispatcherSelector, Terminated }
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise, Await }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.mutable.ListBuffer

object Ring {

  enum Message {
    case Pass(msg: String, count: Int, endTimePromise: Promise[Long])
    case Stop(left: Int)
  }

  def ringMember(
    forwardTo: Future[ActorRef[Message]], members: Int, numMsgs: Int
  ) = Behaviors.setup[Message] { ctx => new MutableRingMember(ctx, forwardTo, members, numMsgs) }

  class MutableRingMember(
    ctx: ActorContext[Message],
    forwardTo: Future[ActorRef[Message]], members: Int, numMsgs: Int
  ) extends AbstractBehavior[Message](ctx) {
    var msgFinished = 0
    var msgDone = 0


    override def onMessage(msg: Message): Behavior[Message] = {
      val ft = Await.result(forwardTo, Duration.Inf)
      msg match {
        case Message.Pass(msg, count, endTimePromise) =>
          if (count > 0) {
            ft ! Message.Pass(msg, count - 1, endTimePromise)
            Behaviors.same
          } else {
            msgFinished += 1
            ft ! Message.Stop(members - 1)
            if (msgFinished == numMsgs) {
              endTimePromise.success(System.nanoTime())
              Behaviors.stopped
            } else {
              Behaviors.same
            }
          }
        case Message.Stop(left) =>
          msgDone += 1
          if (left > 1) {
            ft ! Message.Stop(left - 1)
          }
          if ( msgDone == numMsgs) {
            Behaviors.stopped
          } else {
            Behaviors.same
          }
      }
    }

  }

  def mainActor(
    durationPromise: Promise[Long],
    numRingMembers: Int,
    numHops: Int,
    numMsgs: Int
    ): Behavior[akka.NotUsed] =
    Behaviors.setup { ctx =>

      val endTimePromise = Promise[Long]()
      val endTimeFuture = endTimePromise.future

      val lastRingMemberPromise = Promise[ActorRef[Message]]()
      val lastRingMemberFuture = lastRingMemberPromise.future

      var ring = new ListBuffer[ActorRef[Message]]()

      for (i <- 0 until numRingMembers) {
        if (i == 0) {
          ring += ctx.spawn(ringMember(
            lastRingMemberFuture, numRingMembers, numMsgs),
            "member" + i
          )
        } else {
          ring += ctx.spawn(ringMember(
            Future { ring(i - 1) }, numRingMembers, numMsgs),
            "member" + i
          )
        }
      }

      lastRingMemberPromise.success(ring.last)
      val startTime = System.nanoTime()
      (1 to numMsgs).toList.foreach { _ =>
        ring(0) ! Message.Pass("hello", numHops, endTimePromise)
      }
      val endTime = Await.result(endTimeFuture, Duration.Inf)

      durationPromise.success(endTime - startTime)
      Behaviors.stopped
    }

  def bench(params: (Int, Int, Int)): Long = {
    val (numRingMembers, numHops, numMsgs) = params

    val durationPromise = Promise[Long]()
    val durationFuture = durationPromise.future
    val system = ActorSystem(
      mainActor(durationPromise, numRingMembers, numHops, numMsgs),
      "ThreadRingMultiMessageDemo")
    Await.result(system.whenTerminated, Duration.Inf)
    val duration = Await.result(durationFuture, Duration.Inf)
    duration
  }

}

