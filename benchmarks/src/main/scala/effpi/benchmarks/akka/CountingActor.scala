// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.akka

import akka.NotUsed
import akka.actor.typed.scaladsl.{ Behaviors, AbstractBehavior, ActorContext}
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior, Terminated }

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise, Await }
import scala.concurrent.ExecutionContext.Implicits.global

object CountingActor {

  sealed trait CounterAction
  object CounterAction {
    final case class Add(num: Int, p: Promise[Int]) extends CounterAction
    final case class Cheque(replyTo: ActorRef[Sum]) extends CounterAction
  }

  case class Sum(sum: Int)

  val counter = Behaviors.setup[CounterAction] { ctx =>
    new MutableCounter(ctx)
  }

  class MutableCounter(
    ctx: ActorContext[CounterAction]
  ) extends AbstractBehavior[CounterAction](ctx) {
    var counter = 0

    override def onMessage(msg: CounterAction): Behavior[CounterAction] = {
      msg match {
        case CounterAction.Add(num, p) =>
          counter += 1
          p.success(num)
          Behaviors.same
        case CounterAction.Cheque(replyTo) =>
          replyTo ! Sum(counter)
          Behaviors.stopped
      }
    }
  }

  def sink(endTimePromise: Promise[Long]) = Behaviors.receive[Sum] { (ctx, msg) =>
    endTimePromise.success(System.nanoTime())
    Behaviors.stopped
  }

  def mainActor(
    durationPromise: Promise[Long],
    numMessages: Int
  ): Behavior[akka.NotUsed] =
    Behaviors.setup { ctx =>

      val endTimePromise = Promise[Long]()
      val endTimeFuture = endTimePromise.future

      val sinkRef = ctx.spawn(sink(endTimePromise), "sink")
      ctx.watch(sinkRef)
      val counterRef = ctx.spawn(counter, "counter")

      val startTime = System.nanoTime()
      val futs = (1 to numMessages).toList.map { num =>
        val p = Promise[Int]()
        val f = p.future
        counterRef ! CounterAction.Add(num, p)
        f
      }

      Await.result(Future.sequence(futs), Duration.Inf)

      counterRef ! CounterAction.Cheque(sinkRef)

      val endTime = Await.result(endTimeFuture, Duration.Inf)
      val countingDuration = endTime - startTime
      durationPromise.success(countingDuration)

      Behaviors.receiveSignal {
        case (_, Terminated(ref)) =>
          Behaviors.stopped
        case (_, _) =>
          Behaviors.empty
      }
    }

  def bench(params: Int): Long = {
    val durationPromise = Promise[Long]()
    val durationFuture = durationPromise.future
    val system = ActorSystem(
      mainActor(durationPromise, params), "CountingActorDemo")
    Await.result(system.whenTerminated, Duration.Inf)
    val duration = Await.result(durationFuture, Duration.Inf)
    duration
  }
}

