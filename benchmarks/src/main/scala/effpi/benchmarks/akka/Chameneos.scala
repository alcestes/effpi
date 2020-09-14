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

object Chameneos {

  enum Colour {
    case Blue
    case Red
    case Yellow
  }

  case class Request(replyTo: ActorRef[Response], mate: ActorRef[PtoPCommunication])
  // enum Request {
  //   case To(replyTo: ActorRef[Response], mate: ActorRef[PtoPCommunication])
  // }

  enum Response {
    case Start(hi: Int)
    case Mate(replyTo: ActorRef[PtoPCommunication])
    case Stop
  }

  enum PtoPCommunication {
    case Col(colour: Colour)
    case Stop
  }

  def server(maxMeetings: Int, numChameneos: Int,
    startTimeFuture: Future[Long], endTimePromise: Promise[Long]) =
    Behaviors.setup[Request] { ctx =>
      new MutableServer(ctx, maxMeetings, numChameneos, startTimeFuture, endTimePromise)
    }

  class MutableServer(
    ctx: ActorContext[Request],
    maxMeetings: Int, numChameneos: Int,
    startTimeFuture: Future[Long], endTimePromise: Promise[Long]
  ) extends AbstractBehavior[Request](ctx) {

    var mate: Option[Request] = None
    var meetings = 0
    var stoppedChameneos = 0

    override def onMessage(msg: Request): Behavior[Request] = {
      Await.result(startTimeFuture, Duration.Inf)
      mate match {
        case None =>
          if (meetings < maxMeetings) {
            mate = Some(msg)
            this
          } else {
            if (stoppedChameneos == 0) {
              endTimePromise.success(System.nanoTime())
            }
            msg.replyTo ! Response.Stop
            stoppedChameneos += 1
            if (stoppedChameneos < numChameneos) {
              this
            } else {
              Behaviors.stopped
            }
          }
        case Some(r) =>
          r.replyTo ! Response.Mate(msg.mate)
          msg.replyTo ! Response.Mate(r.mate)
          mate = None
          meetings += 1
          this
      }
    }
  }

  def chameneos(server: ActorRef[Request], initColour: Colour, id: Int) =
    Behaviors.setup[Response] { ctx => new MutableChameneos(ctx, server, initColour, id) }

  class MutableChameneos(ctx: ActorContext[Response],
                         server: ActorRef[Request],
                         initColour: Colour,
                         id: Int) extends AbstractBehavior[Response](ctx) {
    var colour = initColour
    val colorer = Behaviors.receive[PtoPCommunication] { (ctx, msg) =>
      msg match {
        case PtoPCommunication.Col(matesColour) =>
            colour = mutatedColour(colour, matesColour)
            Behaviors.same
        case PtoPCommunication.Stop =>
          Behaviors.stopped
      }
    }
    val ref = ctx.spawn(colorer, "colorer" + id)

    override def onMessage(msg: Response): Behavior[Response] = {
      msg match {
        case Response.Start(_) =>
          server ! Request(ctx.self, ref)
          this
        case Response.Mate(mate) =>
          mate ! PtoPCommunication.Col(colour)
          server ! Request(ctx.self, ref)
          this
        case Response.Stop =>
          ref ! PtoPCommunication.Stop
          Behaviors.stopped
      }
    }
  }

  def mutatedColour(colourA: Colour, colourB: Colour) = {
    if (colourA != colourB) {
      (colourA.ordinal + colourB.ordinal) match {
        case 1 => Colour.values(2)
        case 2 => Colour.values(1)
        case 3 => Colour.values(0)
      }
    } else {
      colourA
    }
  }

  def mainActor(
    durationPromise: Promise[Long],
    numChameneos: Int,
    numMeetings: Int
  ): Behavior[akka.NotUsed] =
    Behaviors.setup { ctx =>

      val endTimePromise = Promise[Long]()
      val endTimeFuture = endTimePromise.future
      val startTimePromise = Promise[Long]()
      val startTimeFuture = startTimePromise.future

      val serverRef = ctx.spawn(
        server(numMeetings, numChameneos, startTimeFuture, endTimePromise),
        "server")
      ctx.watch(serverRef)

      val r = new scala.util.Random(99)
      val colours = (1 to numChameneos).toList.map { _ => Colour.values(r.nextInt(3)) }

      val chameneosRefs = colours.zipWithIndex.map { (colour, id) =>
        ctx.spawn(chameneos(serverRef, colour, id), "chameneos" + id)
      }

      chameneosRefs.foreach { chameneosRef =>
        chameneosRef ! Response.Start(0)
      }

      startTimePromise.success(System.nanoTime())

      val startTime = Await.result(startTimeFuture, Duration.Inf)
      val endTime = Await.result(endTimeFuture, Duration.Inf)
      // pick max(endTime - startTime, 0)
      val chameneosDuration = endTime - startTime max 0
      durationPromise.success(chameneosDuration)

      var terminatedProcesses = 0

      Behaviors.receiveSignal {
        case (_, Terminated(ref)) =>
          Behaviors.stopped
        case (_, _) =>
          Behaviors.empty
      }
    }

  def bench(params: (Int, Int)): Long = {
    val (numChameneos, numMeetings) = params
    val durationPromise = Promise[Long]()
    val durationFuture = durationPromise.future
    val system = ActorSystem(
      mainActor(durationPromise, numChameneos, numMeetings),
      "ChameneosDemo")
    Await.result(system.whenTerminated, Duration.Inf)
    val duration = Await.result(durationFuture, Duration.Inf)
    duration
  }
}

