// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.akka

import akka.NotUsed
import akka.actor.typed.scaladsl.{ Behaviors, ActorContext}
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior, DispatcherSelector, Terminated }

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise, Await }
import scala.concurrent.ExecutionContext.Implicits.global

object ForkJoinCreation {

  case class Message(msg: String)

  val simpleActor = Behaviors.receive[Message] { (ctx, msg) =>
    Behaviors.stopped
  }

  def mainActor(
    durationPromise: Promise[Long], numActors: Int
  ): Behavior[akka.NotUsed] =
    Behaviors.setup { ctx =>

      val startTime = System.nanoTime()

      val simpleActorRefs = (1 to numActors).toList.map { id =>
        ctx.spawn(simpleActor, "simple" + id)
      }

      simpleActorRefs.foreach { simpleActorRef =>
        simpleActorRef ! Message("Hello World!")
      }

      val endTime = System.nanoTime()

      durationPromise.success(endTime - startTime)
      Behaviors.stopped
    }

  def bench(params: Int): Long = {
    val durationPromise = Promise[Long]()
    val durationFuture = durationPromise.future
    val system = ActorSystem(
      mainActor(durationPromise, params), "ForkJoinCreationDemo")
    Await.result(system.whenTerminated, Duration.Inf)
    val duration = Await.result(durationFuture, Duration.Inf)
    duration
  }

}
