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
import effpi.process.dsl.{Rec, Loop, rec, loop}
import effpi.system._

object Chameneos {

  implicit val timeout: Duration = Duration.Inf

  enum Colour {
    case Blue
    case Red
    case Yellow
  }

  // Messages
  sealed abstract class Stop()
  case object Stop extends Stop

  case class Mate[+R <: ActorRef[P2PComm]](replyTo: R)
  
  type Response = Mate[ActorRef[P2PComm]] | Stop

  case class Request(replyTo: ActorRef[Response],
                     mate: ActorRef[P2PComm])

  case class MateColour(colour: Colour)
  type P2PComm = MateColour | Stop

  type Server = Rec[RecAt,
                    ReadDep[Request, (x: Request) =>
                              ReadDep[Request, (y: Request) =>
                                SendTo[x.replyTo.type, Mate[y.mate.type]] >>:
                                SendTo[y.replyTo.type, Mate[x.mate.type]] >>:
                                Loop[RecAt] ]]
                    | PNil >>: // FIXME: needed to help type inference :-\
                      ReadDep[Request, (x: Request) =>
                                SendTo[x.replyTo.type, Stop] >>:
                                (Loop[RecAt] | PNil)]]

  def server(maxMeetings: Int, numChameneos: Int)
            (startTimeFuture: Future[Long],
             endTimePromise: Promise[Long]) = Behavior[Request, Server] {
    var meetings = 0
    var stoppedChameneos = 0
    rec(RecA) {
      Await.result(startTimeFuture, Duration.Inf)
      if (meetings < maxMeetings) {
        read { x =>
          read { y =>
            dsl.seq(
              send(x.replyTo, Mate[y.mate.type](y.mate)),
              send(y.replyTo, Mate[x.mate.type](x.mate)),
              {
                meetings += 1
                loop(RecA)
              }
            )
          }
        }
      } else {
        if (stoppedChameneos == 0) {
          endTimePromise.success(System.nanoTime())
        }
        nil >> read { x =>
          dsl.seq(
            send(x.replyTo, Stop),
            {
              stoppedChameneos += 1
              if (stoppedChameneos < numChameneos) {
                loop(RecA)
              } else {
                nil
              }
            }
          )
        }
      }
    }
  }

  type Chameneos[Srv <: ActorRef[Request]] =
       Rec[RecAt,
           Spawn[PtoPChameneos,
                 SendTo[Srv, Request] >>:
                 ReadDep[Response, (x: Response) =>
                         (SendTo[ActorRef[P2PComm], MateColour] >>:
                          Loop[RecAt])
                         | SendTo[ActorRef[P2PComm], Stop]]]]

  type PtoPChameneos = Read[P2PComm, PNil]

  def chameneos(server: ActorRef[Request])(initColour: Colour) =
    Behavior[Response, Chameneos[server.type]] {
      var colour = initColour
      // println(s"Initial colour for $self is $colour")
      rec(RecA) {
        spawn (Behavior[P2PComm, PtoPChameneos]{
          read {
            case MateColour(col) =>
              colour = mutatedColour(colour, col)
              // println(s"The new colour for $self is $colour")
              nil
            case Stop =>
              nil
          }
        }) { ref => dsl.seq(
          send(server, Request(self, ref)),
          read {
            case Mate(mate) =>
              send(mate, MateColour(colour)) >>
                loop(RecA)
            case Stop =>
              send(ref, Stop)
          }
        )}
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


  def bench(param: (Int, Int), psC: () => ProcessSystem) = {

    val (numChameneos, numMeetings) = param
    implicit val ps = psC()

    val endTimePromise = Promise[Long]()
    val endTimeFuture = endTimePromise.future

    val startTimePromise = Promise[Long]()
    val startTimeFuture = startTimePromise.future
    // val startTime = System.nanoTime()

    // val startSignPromise = Promise[Int]()
    // val startSignFuture = startSignPromise.future

    val serverRef = Actor.spawn(server(numMeetings, numChameneos)(startTimeFuture, endTimePromise))
    val r = new scala.util.Random(99)
    val colours = (1 to numChameneos).toArray.map { _ => Colour.values(r.nextInt(3)) }
    colours.map { colour => Actor.spawn(chameneos(serverRef)(colour)) }

    startTimePromise.success(System.nanoTime())

    val startTime = Await.result(startTimeFuture, Duration.Inf)
    val endTime = Await.result(endTimeFuture, Duration.Inf)
    // pick max(endTime - startTime, 0)
    val chameneosDuration = endTime - startTime max 0
    ps.kill()
    chameneosDuration
  }
}
