// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.demo.audit

import scala.concurrent.duration.Duration

import effpi.actor.ActorRef
import effpi.process._
import effpi.actor.dsl._
import effpi.verifier.verify

package object types {
  // Message classes
  case class Pay(amount: Int, replyTo: ActorRef[Result])

  abstract class Result
  case class Accepted() extends Result
  case class Rejected() extends Result

  case class Audit[P <: Pay](payment: P)

  type Payment[Auditor <: ActorRef[Audit[_]]] = (
    Forever[
      ReadDep[Pay, (p: Pay) =>
        SendTo[p.replyTo.type, Rejected]
        |
        ( SendTo[Auditor, Audit[p.type]] >>:
          SendTo[p.replyTo.type, Accepted] )
      ]
    ]
  )
}

package object implementation {
  import types._
  implicit val timeout: Duration = Duration("30 seconds")

  // Payment service actor
  // The properties below verify that:
  //
  //  * the actor will always eventually input from its mailbox (`mb_`), while
  //    possibly interacting via `aud`
  //
  //  * whenever the actor receives a message from its mailbox (`mb_`), it
  //    will eventually respond, possibly after interacting via `aud`
  //
  //  * while interacting on its mailbox (`mb_`), whenever the actor sends
  //    something on `aud`, then it will also eventually produce an `Accepted`
  //    message (before using `aud` again)
  @verify(property = "reactive(mb_)(aud) && responsive(mb_)(aud) && output_eventually_followed(aud)(Accepted)(mb_)")
  def payment(aud: ActorRef[Audit[_]]): Actor[Pay, Payment[aud.type]] = {
    forever {
      read { (pay: Pay) =>
        if (pay.amount > 42000) {
          send(pay.replyTo, Rejected())
        } else {
          send(aud, Audit(pay)) >>
          send(pay.replyTo, Accepted())
        }
      }
    }
  }

  def auditor: Actor[Audit[_], Process] = forever {
    println(s"Auditor: waiting for notification...")
    read { (audit: Audit[_]) =>
      println(s"Auditor: received notification for payment: ${audit.payment}")
      nil
    }
  }

  def client(id: Int, pay: ActorRef[Pay], amount: Int): Actor[Result, Process] = {
    println(s"Client: trying to pay €${amount}...")
    send(pay, Pay(amount, self)) >>
    read { (result: Result) =>
      result match {
        case Accepted() => {
          println(s"Client ${id}: payment of €${amount} accepted")
        }
        case Rejected() => {
          println(s"Client ${id}: payment of €${amount} rejected")
        }
      }
      nil
    }
  }
}

// To run this example, try:
// sbt "examples/runMain effpi.examples.demo.audit.Main"
object Main {
  import types._
  import implementation._

  def main(): Unit = main(Array())

  def main(args: Array[String]) = {
    implicit val ps = effpi.system.ProcessSystemRunnerImproved()

    println("Running demo...")
    val auditRef = Actor.spawn(Behavior[Audit[_], Process](auditor))
    val paymentRef = Actor.spawn(Behavior[Pay, Process](payment(auditRef)))
    
    for (i <- 1 to 10) {
      Actor.spawn(Behavior[Result, Process](client(i, paymentRef, i * 10000)))
    }

    Thread.sleep(5000) // Wait 5 seconds
    ps.kill()
  }
}


