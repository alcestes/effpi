// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.diningphilo

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.verify

///////////////////////////////////////////////////////////////////////////////
// The following package defines and verifies the types/protocols for
// E.W. Dijkstra's dining philosophers problem.
///////////////////////////////////////////////////////////////////////////////
package object types {
  // Fork protocol. The parameters are channel types, used to signal
  // acquisition and release.
  type Fork[Acquire <: OChan[Unit], Release <: IChan[Unit]] = Rec[RecX,
    Out[Acquire, Unit] >>: In[Release, Unit, (_x: Unit) => Loop[RecX]]
  ]

  // Philosopher protocol. The four parameters are the acquire/release pairs
  // of the first and second fork to pick.
  // The philosopher tries to acquire the first fork, then the second fork,
  // then releases them, and loops.
  type Philo[Pick1 <: IChan[Unit], Drop1 <: OChan[Unit],
             Pick2 <: IChan[Unit], Drop2 <: OChan[Unit]] = Rec[RecX, 
    In[Pick1, Unit, (_f1: Unit) =>
      In[Pick2, Unit, (_f2: Unit) =>
        (Out[Drop1, Unit] >>: Out[Drop2, Unit]) >>: Loop[RecX]
      ]
    ]
  ]

  // Dining philosophers protocol. The parameters are three channel pairs,
  // used as acquire/release channels for three forks; the forks are composed
  // in parallel, and interconnected with three philosophers.
  // Note that we reuse the type aliases for the protocols of Philo and Fork,
  // by instantiating them with the Dining type parameters.
  type Dining[C1pick <: Chan[Unit], C1drop <: Chan[Unit],
              C2pick <: Chan[Unit], C2drop <: Chan[Unit],
              C3pick <: Chan[Unit], C3drop <: Chan[Unit]] =
    Par6[ Philo[C3pick, C3drop, C1pick, C1drop],
          Fork[C1pick, C1drop],
          Philo[C1pick, C1drop, C2pick, C2drop],
          Fork[C2pick, C2drop],
          Philo[C2pick, C2drop, C3pick, C3drop],
          Fork[C3pick, C3drop] ]
  
  // To check whether the Dining protocol above is "correct", it is not
  // necessary to implement it: one can define an unimplemented stub method
  // (like diningCHK below), and annotate it for analysis by the effpi
  // compiler plugin.
  @verify(property = "deadlock_free()") // Check fails: the protocol deadlocks!
  def diningCHK(p1: Chan[Unit], d1: Chan[Unit],
                p2: Chan[Unit], d2: Chan[Unit],
                p3: Chan[Unit], d3: Chan[Unit]): Dining[p1.type, d1.type,
                                                        p2.type, d2.type,
                                                        p3.type, d3.type] = ???

  // Deadlock-free dining philosophers protocol.
  // The difference with the Dining protocol above is that the fork channels
  // for the first philosopher are swapped.
  type DiningDF[C1pick <: Chan[Unit], C1drop <: Chan[Unit],
                C2pick <: Chan[Unit], C2drop <: Chan[Unit],
                C3pick <: Chan[Unit], C3drop <: Chan[Unit]] =
    Par6[ Philo[C1pick, C1drop, C3pick, C3drop],
          Fork[C1pick, C1drop],
          Philo[C1pick, C1drop, C2pick, C2drop],
          Fork[C2pick, C2drop],
          Philo[C2pick, C2drop, C3pick, C3drop],
          Fork[C3pick, C3drop] ]

  // As above, to check whether the DiningDF protocol is "correct", one can
  // define an unimplemented stub method, and annotate it for analysis by the
  // effpi compiler plugin
  @verify(property = "deadlock_free()") // Success: protocol is deadlock-free
  def diningDFCHK(p1: Chan[Unit], d1: Chan[Unit],
                  p2: Chan[Unit], d2: Chan[Unit],
                  p3: Chan[Unit], d3: Chan[Unit]): DiningDF[p1.type, d1.type,
                                                            p2.type, d2.type,
                                                            p3.type, d3.type] = ???
}

///////////////////////////////////////////////////////////////////////////////
// Once the types above have been defined and verified, they can be imported
// and used to guide correct implementations, as shown in the package below.
///////////////////////////////////////////////////////////////////////////////
package object implementation {
  import types.{Fork, Philo, Dining, DiningDF}
  import scala.concurrent.duration.Duration

  implicit val timeout: Duration = Duration(30, "seconds")

  // Fork implementation. Its arguments are channels, to acquire and release
  // the fork. The return type annotation instantiates the type Fork above,
  // with types depending on the function arguments: hence, fork() must use
  // its acquire/release channels as described in type Fork.
  def fork(id: Int,
           acquire: OChan[Unit],
           release: IChan[Unit]): Fork[acquire.type, release.type] = rec(RecX) {
    println(s"Fork ${id}: available")
    send(acquire, ()) >> {
      println(s"Fork ${id}: picked")
      receive(release) { _ =>
        loop(RecX)
      }
    }
  }

  // Philosopher implementation. Its arguments are channels, used to
  // pick/release the forks. As in fork() above, the return type
  // depends on the function arguments, and enforces their use.
  def philo(name: String,
            pick1: IChan[Unit],
            drop1: OChan[Unit],
            pick2: IChan[Unit],
            drop2: OChan[Unit]): Philo[pick1.type, drop1.type,
                                       pick2.type, drop2.type] = rec(RecX) {
    println(s"${name}: picking first fork...")
    receive(pick1) { _ =>
      println(s"${name}: picking second fork...")
      receive(pick2) { _ =>
        println(s"${name}: eating, then dropping forks...")
        send(drop1, ()) >> send(drop2, ()) >> {
          println(s"${name}: Thinking...")
          loop(RecX)
        }
      }
    }
  }

  // Dining philosophers implementation. As above, the return type dictates
  // how the function arguments are used.
  @verify(property = "deadlock_free()") // Fails: implementation may deadlock!
  def dining(p1: Chan[Unit], d1: Chan[Unit],
             p2: Chan[Unit], d2: Chan[Unit],
             p3: Chan[Unit], d3: Chan[Unit]): Dining[p1.type, d1.type,
                                                     p2.type, d2.type,
                                                     p3.type, d3.type] = {
    println("Dining philosophers - deadlocking implementation")
    par(philo("Socrates", p3, d3, p1, d1),
       fork(1, p1, d1),
       philo("Aristotle", p1, d1, p2, d2),
       fork(2, p2, d2),
       philo("Plato", p2, d2, p3, d3),
       fork(3, p3, d3) )
  }

  // Deadlock-free dining philosophers implementation. As above, the return
  // type annotation dictates how the argument channels are used;
  // hence, the first philosopher is instantiated with "swapped" forks
  // wrt. dining() above.
  // If the forks are passed in the wrong order, a compilation error ensues.
  //
  // NOTE: if the type annotation "DiningDF[...]" is removed, then the code
  // still compiles --- since Dotty is able to infer a type, thus ensuring type
  // safety; however, the inferred type will likely be a supertype of
  // DiningDF[...], not precise enough for accurate verification.
  //
  // In general, relying on types inferred by Dotty may have two drawbacks:
  //
  // (A) the effpi compiler plugin may be unable to understand and verify
  //     the inferred behavioural type (it happens in this example); or
  //
  // (B) the inferred behavioural type may be very over-approximating
  //     (cf. Example 3.5 in the paper), and its verification will be sound
  //     but inaccurate (i.e., model checking will always return "false"
  //     for liveness properties).
  @verify(property = "deadlock_free()") // Succeeds: no run-time deadlocks
  def diningDF(p1: Chan[Unit], d1: Chan[Unit],
               p2: Chan[Unit], d2: Chan[Unit],
               p3: Chan[Unit], d3: Chan[Unit]): DiningDF[p1.type, d1.type,
                                                         p2.type, d2.type,
                                                         p3.type, d3.type] = {
    println("Dining philosophers - deadlock-free implementation")
    par(philo("Socrates", p1, d1, p3, d3),
        fork(1, p1, d1),
        philo("Aristotle", p1, d1, p2, d2),
        fork(2, p2, d2),
        philo("Plato", p2, d2, p3, d3),
        fork(3, p3, d3) )
  }
}

// To run this example, try:
// sbt "examples/runMain effpi.examples.diningphilo.Main"
object Main {
  import implementation.{dining, diningDF}
  def main(): Unit = main(Array())

  def main(args: Array[String]) = {
    val (c1pick, c1drop) = (Chan[Unit](), Chan[Unit]())
    val (c2pick, c2drop) = (Chan[Unit](), Chan[Unit]())
    val (c3pick, c3drop) = (Chan[Unit](), Chan[Unit]())

    eval(
      // Here we can test dining() or diningDF() above
      // NOTE: diningDF() will timeout after 30 seconds after getting stuck
      diningDF(c1pick, c1drop, c2pick, c2drop, c3pick, c3drop)
    )
  }
}
