// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.demo.pingpong

import scala.concurrent.duration.Duration

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.verify

package object types {
  type Pinger = [P <: Chan[String], C <: OChan[P]] => (
    Out[C, P] >>: In[P, String, (x: String) => PNil]
  )

  type Ponger = [C <: IChan[OChan[String]]] => (
    In[C, OChan[String], (x: OChan[String]) => Out[x.type, String]]
  )

  type PingPong = [P1 <: Chan[OChan[String]], P2 <: Chan[String]] => (
    Par[Ponger[P1], Pinger[P2, P1]]
  )
}

package object implementation {
  import types._

  implicit val timeout: Duration = Duration("30 seconds")

  def pinger(self: Chan[String],
             pongc: OChan[OChan[String]]): Pinger[self.type, pongc.type] = {
    println("Pinger: sending reply channel...")
    send(pongc, self) >> {
      println("Waiting for answer...")
      receive(self) { reply =>
        println(s"Pinger: ...received: ${reply}")
        nil
      }
    }
  }

  // This annotation checks whether the ponger process will always answer back
  // when it receives a message.
  @verify(property = "responsive(self)")
  def ponger(self: IChan[OChan[String]]): Ponger[self.type] = {
    println("Ponger: waiting for channel...")
    receive(self) { reqc =>
      println(s"Ponger: ...received: ${reqc}. Sending reply.")
      send(reqc, "Hello!")
    }
  }

  // This annotation checks whether p2 is eventually used for output. Note that
  // p2 is sent from pinger to ponger before being used, and the analysis can
  // track it.
  @verify(property = "eventual_output_use(p2)")
  def pingpong(p1: Chan[OChan[String]],
               p2: Chan[String]): PingPong[p1.type, p2.type] = {
    par(ponger(p1), pinger(p2, p1))
  }
}

// To run this example, try:
// sbt "examples/runMain effpi.examples.demo.pingpong.Main"
object Main {
  import implementation.pingpong
  def main(): Unit = main(Array())

  def main(args: Array[String]) = {
    val (c1, c2) = (Chan[OChan[String]](), Chan[String]())

    eval(
      pingpong(c1, c2)
    )
  }
}
