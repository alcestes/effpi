// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.verifier

import effpi.channel._
import effpi.process._
import effpi.process.dsl._
import effpi.verifier.verify
import scala.concurrent.duration.Duration

object ProcessVerifExamples {
  //@verify(property = "FIXME")
  def test1(i: InChannel[OutChannel[Int]]): In[i.type, OutChannel[Int], (o: OutChannel[Int]) => Out[o.type, Int]] = ???

  type Type2RecX[C <: OutChannel[Int]] = Out[C, Int] >>: Loop[RecX]
  type Type2[C <: OutChannel[Int]] = Rec[RecX, Type2RecX[C]]

  def test2recx(c: OutChannel[Int]): Type2RecX[c.type] = {
    if (scala.util.Random.nextBoolean) {
      send(c, 42) >>
      loop(RecX)
    } else {
      send(c, 43) >>
      loop(RecX)
    }
  }

  //@verify(property = "FIXME")
  def test2(c: OutChannel[Int]): Type2[c.type] = {
    rec(RecX)(test2recx(c))
  }

  // Process spawning & connections
  type Process3[C1 <: Channel[Int], C2 <: Channel[String]] =
       Fork[Process3Fork[C1, C2]] >>: Process3Cont[C1, C2]
  type Process3Fork[C1 <: Channel[Int], C2 <: Channel[String]] = In[C1, Int, (x: Int) => Out[C2, String]]
  type Process3Cont[C1 <: Channel[Int], C2 <: Channel[String]] = Out[C1, Int] >>: In[C2, String, (x: String) => PNil]

  def test3spawn(c1: Channel[Int], c2: Channel[String])
                (implicit timeout: Duration): Process3Fork[c1.type, c2.type] = {
    println(s"P1: Waiting for input")
    receive(c1) { x =>
      println(s"P1: Received: ${x}; sending 'Thanks'")
      send(c2, "Thanks")
    }
  }

  def test3cont(c1: Channel[Int], c2: Channel[String])
               (implicit timeout: Duration): Process3Cont[c1.type, c2.type] = {
    println("P2: Sending 42, waiting for answer")
    send(c1, 42) >>
    receive(c2) { (x: String) =>
      println(s"P2: Received: ${x}")
      nil
    }
  }

  //@verify(property = "FIXME")
  def test3(c1: Channel[Int], c2: Channel[String])
           (implicit timeout: Duration): Process3[c1.type,
                                                  c2.type] = ???
  //{
  //  fork(test3spawn(c1, c2)) >>
  //  test3cont(c1, c2)
  //}
  
  @verify(property = "no_output_use(c2)")
  def test_no_output_use(c1: Channel[Int],
                         c2: Channel[String]): Out[c1.type, Int] = ???

  @verify(property = "eventual_output_use(c1)(c2)")
  def test_eventual_output_use(c1: Channel[Int],
                               c2: Channel[Int]): In[c2.type, Int,
                                                     (x: Int) => Out[c1.type, x.type]] = ???

  @verify(property = "forwarding(c1)(c2)")
  def test_forwarding(c1: Channel[Int],
                      c2: Channel[Int], y: Int): In[c1.type, Int,
                                            (x: Int) => Out[c2.type, x.type]] = ???
}
