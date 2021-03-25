// Effpi - verified message-passing programs in Dotty
// Copyright 2021 Alceste Scalas
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.demo.atm

import scala.concurrent.duration.Duration

import effpi.process._
import effpi.process.dsl._
import effpi.channel.{Channel => Chan}

case class Balance()
case class Pay()
case class Quit()

type Request = Balance | Pay | Quit

case class Report(data: String)
case class OK(message: String)
case class Fail(error: String)

type Response = Report | OK | Fail

type ATM[Ci <: Chan[Request], Co <: Chan[Response]] = (
  In[Ci, Request, (r: Request) => ReqMatch[Ci, Co, r.type]]
)

type ReqMatch[Ci <: Chan[Request], Co <: Chan[Response],
              R <: Request] <: Process = R match {
  case Balance => Out[Co, Report]
  case Pay     => Out[Co, OK|Fail]
  case Quit    => PNil
}


def atm(ci: Chan[Request], co: Chan[Response])
       (implicit timeout: Duration): ATM[ci.type, co.type] = {
  receive(ci) {
    case _: Balance => {
      println("Report requested")
      send(co, Report("Your balance is €1.00"))
    }
    case _: Pay => {
      println("Payment requested")
      if (coinToss()) {
        send(co, OK("Payment successful"))
      } else {
        send(co, Fail("Payment rejected!"))
      }
    }
    case _: Quit => {
      println("Ending the session")
      nil
    }
  }
}

def coinToss(): Boolean = true

// To run this example, try:
// sbt "examples/runMain effpi.examples.demo.atm.Main"
object Main {
  def main(args: Array[String]) = {
    implicit val ps = effpi.system.ProcessSystemRunnerImproved()

    Thread.sleep(5000) // Wait 5 seconds
    ps.kill()
  }
}
