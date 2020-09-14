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

object CountingActor {

  implicit val timeout: Duration = Duration.Inf

  sealed trait CounterAction
  object CounterAction {
    final case class Add(num: Int) extends CounterAction
    final case class Cheque(replyTo: ActorRef[Sum]) extends CounterAction
  }

  case class Sum(sum: Int)

  type Counter = Rec[RecAt, Read[CounterAction, (Loop[RecAt] | SendTo[ActorRef[Sum], Sum])]]

  val counter = Behavior[CounterAction, Counter] {
    var counter = 0
    prec(RecA) {
      read {
        case CounterAction.Add(num) =>
          counter += num
          ploop(RecA)
        case CounterAction.Cheque(replyTo) =>
          send(replyTo, Sum(counter))
      }
    }
  }

  type Contributor = Rec[RecAt, SendTo[ActorRef[CounterAction], CounterAction] >>: (Loop[RecAt] | Read[Sum, PNil])]

  def contributor(elemNum: Int, counter: ActorRef[CounterAction])(startTimePromise: Promise[Long], endTimePromise: Promise[Long]) = Behavior[Sum, Contributor] {
    startTimePromise.success(System.nanoTime())
    var num = 0
    prec(RecA) {
      if (num < elemNum) {
        num += 1
        send(counter, CounterAction.Add(num)) >>
          ploop(RecA)
      } else {
        send(counter, CounterAction.Cheque(self)) >>
        read { (x: Sum) => x match {
            case Sum(sum) =>
              endTimePromise.success(System.nanoTime())
              // println(s"The sum of all numbers is $sum")
              nil
          }}
      }
    }
  }

  def bench(numMessages: Int, psC: () => ProcessSystem): Long = {

    implicit val ps = psC()
    val startTimePromise = Promise[Long]()
    val startTimeFuture = startTimePromise.future

    val endTimePromise = Promise[Long]()
    val endTimeFuture = endTimePromise.future

    val counterRef = Actor.spawn(counter)
    Actor.spawn(contributor(numMessages, counterRef)(startTimePromise, endTimePromise))

    val startTime = Await.result(startTimeFuture, Duration.Inf)
    val endTime = Await.result(endTimeFuture, Duration.Inf)
    val countingDuration = endTime - startTime
    ps.kill()
    countingDuration
  }

}
