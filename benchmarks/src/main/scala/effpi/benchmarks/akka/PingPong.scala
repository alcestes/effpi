// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.benchmarks.akka

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior, Terminated }
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.{ Future, Promise, Await }
import scala.concurrent.ExecutionContext.Implicits.global

object PingPong {

  final case class Ping(iterations: Int, replyTo: ActorRef[Pong])

  case class Pong(iterations: Int, pingTo: ActorRef[Ping])

  val pong = Behaviors.receive[Ping] { (ctx, msg) =>
    msg.replyTo ! Pong(msg.iterations - 1, ctx.self)
    Behaviors.same
  }

  def ping(startTimePromise: Promise[Long], endTimePromise: Promise[Long], expectedIterations: Int) = Behaviors.receive[Pong] { (ctx, pong) =>
    if (pong.iterations == 0) {
      endTimePromise.success(System.nanoTime())
      Behaviors.stopped
    } else {
      if (expectedIterations == pong.iterations) {
        startTimePromise.success(System.nanoTime())
      }
      pong.pingTo ! Ping(pong.iterations, ctx.self)
      Behaviors.same
    }
  }

  def mainActor(
    durationPromise: Promise[Long],
    numPairs: Int,
    numIterations: Int
  ): Behavior[akka.NotUsed] =
    Behaviors.setup { ctx =>

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

      // val refs = (1 to numPairs).toList.map { id =>
      val refs = startTimePromises.zip(endTimePromises).zipWithIndex.map { (promises, id) =>
        val (sPromise, ePromise) = promises
        val pongRef = ctx.spawn(pong, "pong" + id)
        val pingRef = ctx.spawn(ping(sPromise, ePromise, numIterations), "ping" + id)
        ctx.watch(pingRef)
        (pingRef, pongRef)
      }
      refs.foreach { (pingRef, pongRef) => pingRef ! Pong(numIterations, pongRef) }

      val startTimes = Await.result(Future.sequence(startTimeFutures), Duration.Inf)
      val startTime = startTimes.min
      val endTimes = Await.result(Future.sequence(endTimeFutures), Duration.Inf)
      val endTime = endTimes.max
      durationPromise.success(endTime - startTime)
      val pingPongDuration = endTime - startTime

      var terminatedProcesses = 0
      Behaviors.receiveSignal {
        case (_, Terminated(ref)) =>
          terminatedProcesses = terminatedProcesses + 1
          if (terminatedProcesses == numPairs) {
            Behaviors.stopped
          } else {
            Behaviors.same
          }
          Behaviors.stopped
        case (_, _) =>
          Behaviors.empty
      }
    }

  def bench(params: (Int, Int)): Long = {
    val (numPairs, numIterations) = params
    val durationPromise = Promise[Long]()
    val durationFuture = durationPromise.future
    val system = ActorSystem(
      mainActor(durationPromise, numPairs, numIterations), "PingPongDemo")
    Await.result(system.whenTerminated, Duration.Inf)
    val duration = Await.result(durationFuture, Duration.Inf)
    duration
  }
}

