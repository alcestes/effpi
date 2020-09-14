// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.filterserver

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.verify

package object types {
  // Filter protocol
  type Filter[I1 <: IChan[Int], I2 <: IChan[Int], O <: OChan[Int]] = Rec[RecX,
    In[I1, Int, (x: Int) => ( In[I2, Int, (y: Int) =>
      (Out[O, x.type] | Out[O, y.type]) >>: Loop[RecX] ])]
  ]

  // Type for mobile filter code
  type FilterCode = (i1: Chan[Int], i2: IChan[Int], o: OChan[Int]) => (
    Filter[i1.type, i2.type, o.type]
  )

}

///////////////////////////////////////////////////////////////////////////////
// Once the types above have been defined and verified, they can be imported
// and used to guide correct implementations, as shown in the package below.
///////////////////////////////////////////////////////////////////////////////
package object implementation {
  import types._
  import scala.concurrent.duration.Duration

  implicit val timeout: Duration = Duration(30, "seconds")

  def filter1(i1: IChan[Int], i2: IChan[Int],
              o: OChan[Int]): Filter[i1.type, i2.type, o.type] = {
    var lastForwarded = i2 // Last used for forwarding, between i1 or i2
    rec(RecX) {
      println("Filter1: waiting for data from 1st channel...")
      receive(i1) { x =>
        println(s"Filter1: ...received ${x}. Waiting for data from 2nd channel...")
        receive(i2) { y =>
          println(s"Filter1: ...received ${y}.")
          if (lastForwarded == i2) {
            println(s"Filter1: Forwarding ${x}, and looping")
            lastForwarded = i1
            send(o, x) >>
            loop(RecX)
          } else {
            println(s"Filter1: Forwarding ${y}, and looping")
            lastForwarded = i2
            send(o, y) >>
            loop(RecX)
          } 
        }
      }
    }
  }

  def filter2(i1: IChan[Int], i2: IChan[Int],
              o: OChan[Int]): Filter[i1.type, i2.type, o.type] = rec(RecX) {
    println("Filter2: waiting for data from 1st channel...")
    receive(i1) { x =>
      println(s"Filter2: ...received ${x}. Waiting for data from 2nd channel...")
      receive(i2) { y =>
        println(s"Filter2: ...received ${y}. Sending the largest, and looping")
        if (x > y) {
          send(o, x) >>
          loop(RecX)
        } else {
          send(o, y) >>
          loop(RecX)
        } 
      }
    }
  }

  // Data producer, used by `server` below
  private def producer(out: OChan[Int]) = rec(RecX) {
    send(out, scala.util.Random.nextInt) >>
    loop(RecX)
  }

  // Data filtering server, waiting for mobile filtering code on `cm`, and
  // running it.
  def server(cm: IChan[FilterCode], o: OChan[Int]) = {
    val (data1, data2) = (Chan[Int](), Chan[Int]())
    par(
      producer(data1),
      producer(data2),
      receive(cm) { (code: FilterCode) => 
        code(data1, data2, o)
      }
    )
  }
} 

// To run this example, try:
//   sbt "examples/runMain effpi.examples.filterserver.Main"
// NOTE: this will loop forever; you will need to press Ctrl+C to stop it
object Main {
  import types._
  import implementation._
  def main(): Unit = main(Array())

  def main(args: Array[String]) = {
    val cm = Chan[FilterCode]()
    val outputs = Chan[Int]()

    val filter = (i1: Chan[Int], i2: IChan[Int], o: OChan[Int]) => {
       // We can pick filter1 or filter2 above (or others, as long as they
       // have type Filter, and can be sent over channel `cm`. Otherwise, the
       // Dotty compiler will issue a type error
      filter2(i1, i2, o)
    }

    eval(
      par(
        send(cm, filter), // Client sending mobile code
        server(cm, outputs), // Server receiving mobile code, and running it
        rec(RecX) { // A process that just consumes the outputs
          receive(outputs) { _ => loop(RecX) }
        }
      )
    )
  }
}
