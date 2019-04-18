// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
//package effpi.test.process

//object Tests {
//  import effpi.process.dsl._
//  import effpi.channel._
//  import scala.concurrent.duration.Duration

//  abstract class C1[I,O]() extends Channel[I,O]
//  abstract class C2[I,O]() extends Channel[I,O]
//  abstract class C3() extends UChannel
//  abstract class C4() extends UChannel

//  // Messages
//  case class A()
//  case class B()
//  case class C()
//  case class D()

//  implicit val timeout: Duration = Duration(30, "seconds")

//  type Process1[C1 <: OutChannel[A|B], C2 <: InChannel[A]] =
//    (Out[C1, A] >>: In[C2, A, PNil]) | Out[C1, B]

//  def subtest1a(x: A) = {
//    println(s"Received: ${x}")
//    nil
//  }
//  def test1a(c1: C1[_,A|B], c2: C2[A,_]): Process1[c1.type, c2.type] = {
//    send(c1, A()) >> receive(c2)(subtest1a)
//  }

//  def test1b(c: C1[_,A|B]): Process1[c.type, InChannel[A]] = {
//    send(c, B())
//  }

//  def test1c(c1: C1[_,A|B], c2: C2[A,_]): Process1[c1.type, c2.type] = {
//    if (scala.util.Random.nextBoolean) {
//      send(c1, A()) >>
//      receive(c2) { x:A =>
//        println(s"Received: ${x}")
//        nil
//      }
//    } else {
//      send(c1, B())
//    }
//  }

//  // Dependencies
//  type Process2[C1 <: Channel[A|B,Int], C2 <: InChannel[A]] =
//    In[C1, A|B, (Dep[B, Out[C1, Int]] |
//                 Dep[A, In[C2, A, PNil]])]

//  def test2(c1: Channel[A|B,Int], c2: Channel[A,_]): Process2[c1.type, c2.type] = {
//    receive(c1) {
//      case m:A => dep(m) {
//        receive(c2) { x =>
//          nil
//        }
//      }
//      case m:B => dep(m) {
//        send(c1, 42)
//      }
//    }
//  }

//  // Process spawning & connections
//  type Process3[C1 <: Channel[Int,String], C2 <: Channel[String,Int]] =
//    Fork[In[C1, Int, Out[C1, String]]] >>: Out[C2, Int] >>: In[C2, String, PNil]

//  def test3spawn(c: Channel[Int, String]): In[c.type, Int, Out[c.type, String]] = {
//    println(s"P1: Waiting for input")
//    receive(c) { x =>
//      println(s"P1: Received: ${x}; sending 'Thanks'")
//      send(c, "Thanks")
//    }
//  }
//  def test3(conn: Connection[Int, String]): Process3[conn.c1.type,
//                                                     conn.c2.type] = {
//    fork(test3spawn(conn.c1)) >> {
//      println("P2: Sending 42, waiting for answer")
//      send(conn.c2, 42) >>
//      receive(conn.c2) { x =>
//        println(s"P2: Received: ${x}")
//        nil
//      }
//    }
//  }

//  // Structural recursion experiments
//  /*
//  sealed trait T1 {type M1}
//  sealed trait T2 {type M2}
//  type TestT1 <: T1 {type M1 = TestT2}
//  type TestT2 <: T2 {type M2 = TestT1}
//  */
//  //type Process4 = Out[R1, Int, Process4] // Forbidden: cyclic reference :-(

//  // Recursion
//  type Process4 = Rec[RecXt, Loop[RecXt]]
//  def test4: Process4 = {
//    var n = 0
//    rec(RecX) {
//      if (n < 5) {
//        println(s"n = ${n}: incrementing and looping")
//        n += 1
//        loop(RecX)
//      } else {
//        println(s"n = ${n}: throwing exception to terminate")
//        throw new RuntimeException("Completed")
//      }
//    }
//  }

//  def test4mem: Process4 = {
//    var n: Long = 0
//    rec(RecX) {
//      if ((n % 100000000) == 0) {
//        val mem = Runtime.getRuntime().totalMemory()
//        println(s"n = ${n}; totalMemory: ${mem}")
//      }
//      // if ((n % 100000000) == 0) {
//      //   println("Triggering garbage collector"); println
//      //   System.gc()
//      // }
//      n += 1
//      loop(RecX)
//    }
//  }

//  type Process5RecX[C <: OutChannel[Int|String]] =
//    (Out[C, Int] >>: Loop[RecXt]) | Out[C, String]
//  type Process5[C <: OutChannel[Int|String]] = Rec[RecXt, Process5RecX[C]]

//  def test5recx(c: OutChannel[Int|String]): Process5RecX[c.type] = {
//    if (scala.util.Random.nextBoolean) {
//      send(c, 42) >>
//      loop(RecX)
//    } else {
//      send(c, "Bye")
//    }
//  }
//  def test5(c: OutChannel[Int|String]): Process5[c.type] = {
//    rec(RecX)(test5recx(c))
//  }

//  type Process6DefX[C <: OutChannel[Int|String]] =
//    Out[C, Int] >>: Call[ProcX, Int] | Out[C, String]
//  type Process6[C <: OutChannel[Int|String]] = Def[ProcX, Int, Process6DefX[C],
//                                                   Call[ProcX, Int]]

//  def test6defx(c: OutChannel[Int|String])(n: Int): Process6DefX[c.type] = {
//    if (n < 10) {
//      send(c, n) >>
//      pcall(ProcX(), n+1)
//    } else {
//      send(c, "Bye")
//    }
//  }
//  def test6(c: OutChannel[Int|String]): Process6[c.type] = {
//    pdef(ProcX())(test6defx(c)) {
//      pcall(ProcX(), 0)
//    }
//  }
//}

//object Main {
//  import effpi.channel._
//  import effpi.process.dsl
//  import Tests._

//  def main(args: Array[String]) = {
//    println("Running tests...")
//    val conn = QueueChannel.connection[Int,String]
//  	val tests = List(//test1a(C1Impl(), C2Impl()),
//                     //test1b(C1Impl()),
//                     //test1c(C1Impl(), C2Impl()),
//                     //test2(C1Impl(), C2Impl()),
//                     test3(conn)
//                     //test4,
//                     //test5(C1Impl()),
//                     //test6(C1Impl())
//                     //,test4mem
//                    )
//    for {t <- tests} { dsl.eval(t); println }
//  }
//}
