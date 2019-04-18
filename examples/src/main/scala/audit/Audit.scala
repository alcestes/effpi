// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.audit

import effpi.actor.ActorRef
import effpi.actor.dsl._
import effpi.process._
import effpi.process.dsl.{Yielding, pyield}

import java.net.URI
import java.util.UUID
import scala.util.Random
import scala.concurrent.duration.Duration
import scala.language.implicitConversions

object GetMoney {
  implicit val timeout: Duration = Duration.Inf

  // Process specifications
  type GetMoney[Pay <: ActorRef[PaymentService],
                Aud <: ActorRef[LogActivity], R] =
         Forever[Read[ReqMoney[R],
                      Audit[Aud] >>: Payment[Pay] >>: Audit[Aud] >>: Send[R]]]

  type Audit[Log <: ActorRef[LogActivity]] =
         SendTo[Log, LogActivity] >>: Read[ActivityLogged, Yield[Long]]

  type Payment[Pay <: ActorRef[PaymentService]] =
         SendTo[Pay, Authorize] >>:
         Read[PaymentResult, SendTo[Pay, Capture] >>:
                             Read[PaymentResult, PNil]]

  def getMoney[R](payments: ActorRef[PaymentService],
                  aud: ActorRef[LogActivity]) = Behavior[ReqMoney[R], GetMoney[payments.type, aud.type, R]] {
    forever {
      read { req =>
        call(doAudit(aud, self, "starting payment")) { id =>
          println(s"Payment started with audit id: ${id}")
          call(doPayment(req.from, req.amount, payments)) >>
          call(doAudit(aud, self, "payment finished")) { id =>
            println(s"Payment finished with audit id: ${id}")
            send(req.replyTo, req.msg)
          }
        }
      }
    }
  }

  def doAudit(aud: ActorRef[LogActivity], who: ActorRef[Nothing],
              msg: String) = Behavior[ActivityLogged, Yielding[Long, Audit[aud.type]]] {
    val id = Random.nextLong()
    send(aud, LogActivity(who, msg, id, self)) >>
    read {
      case ActivityLogged(`who`, `id`) => pyield(id)
    }
  }

  def doPayment(from: URI, amount: BigDecimal,
                payments: ActorRef[PaymentService]) = Behavior[PaymentResult, Payment[payments.type]] {
    val uuid = UUID.randomUUID()
    send(payments, Authorize(from, amount, uuid, self)) >>
    read {
      case PaymentSuccess(`uuid`) => {
        send(payments, Capture(uuid, amount, self)) >>
        read {
          case PaymentSuccess(`uuid`) => nil
        }
      }
    }
  }
}

object Services {
  implicit val timeout: Duration = Duration.Inf

  type Audit = Forever[Read[LogActivity, Send[ActivityLogged]]]

  val audit = Behavior[LogActivity, Audit] {
    println(s"* Audit service started as ${self}")
    forever {
      read { req =>
        println(s"* Logging: ${req}")
        send(req.replyTo, ActivityLogged(req.who, req.id))
      }
    }
  }

  type Payment = Forever[Read[PaymentService, Send[PaymentResult]]]

  val payment = Behavior[PaymentService, Payment] {
    println(s"* Payment service started as ${self}")
    forever {
      read {
        case Authorize(payer, amount, id, replyTo) => {
          println(s"* Authorizing payment: ${amount} from ${payer} (id ${id})")
          send(replyTo, PaymentSuccess(id))
        }
        case Capture(id, amount, replyTo) => {
          send(replyTo, PaymentSuccess(id))
        }
        case Refund(id, replyTo) => {
          send(replyTo, PaymentSuccess(id))
        }
        case Void(id, replyTo) => {
          send(replyTo, PaymentSuccess(id))
        }
      }
    }
  }
}

// To run this example, try:
// sbt "examples/runMain effpi.examples.audit.Main"
object Main {
  import GetMoney._
  import Services._

  def main(): Unit = main(Array())

  def main(args: Array[String]) = {
    import effpi.actor.patterns.ask

    implicit val ps = effpi.system.ProcessSystemRunnerImproved()

    println("Running demo...")
    val auditRef = Actor.spawn(audit)
    val paymentRef = Actor.spawn(payment)
    val b1 = getMoney[String](paymentRef, auditRef)
    val getMoneyRef = Actor.spawn(b1)

    for (i <- 1 to 10) {
      println(s"Sending money request no. ${i}...")
      val result: String = ask(getMoneyRef,
                               ReqMoney(new URI("http://actor.test"), 10000,
                                        s"Payment no. ${i} done", _))
      println(s"...got result: ${result}")
    }

    ps.kill()
  }
}
