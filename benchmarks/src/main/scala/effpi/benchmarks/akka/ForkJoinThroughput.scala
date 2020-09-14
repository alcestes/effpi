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

object ForkJoinThroughput {

  case class Message(msg: String)

  def receiver(maxMsgs: Int) = Behaviors.setup[Message] { ctx =>
    new MutableSimpleActor(ctx, maxMsgs)
  }

  class MutableSimpleActor(
    ctx: ActorContext[Message],
    maxMsgs: Int
  ) extends AbstractBehavior[Message](ctx) {
    var count = 0

    override def onMessage(msg: Message): Behavior[Message] = {
      count +=1
      if (count < maxMsgs) {
        Behaviors.same
      } else {
        Behaviors.stopped
      }
    }
  }

  def mainActor(
    durationPromise: Promise[Long],
    numActors: Int,
    numMessages: Int
  ): Behavior[akka.NotUsed] =
    Behaviors.setup { ctx =>

      val receiversRef = (1 to numActors).map{ id => ctx.spawn(receiver(numMessages), "receiver" + id)}

      val startTime = System.nanoTime()

      (1 to numMessages).foreach { n =>
        receiversRef.foreach { simpleActor =>
          simpleActor ! Message("Hello World!")
        }
      }

      val endTime = System.nanoTime()

      durationPromise.success(endTime - startTime)
      Behaviors.stopped
    }

  def bench(params: (Int, Int)): Long = {
    val (numActors, numMessages) = params
    val durationPromise = Promise[Long]()
    val durationFuture = durationPromise.future
    val system = ActorSystem(
      mainActor(durationPromise, numActors, numMessages),
      "ForkJoinCreationDemo")
    Await.result(system.whenTerminated, Duration.Inf)
    val duration = Await.result(durationFuture, Duration.Inf)
    duration
  }
}

