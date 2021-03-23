// Effpi - verified message-passing programs in Dotty
// Copyright 2020 Alceste Scalas
// Released under the MIT License: https://opensource.org/licenses/MIT

// This file contains experiments toward an API based on for comprehension
// (and some fiddling with match types)
package effpi.forcomp

import effpi.channel._

import scala.concurrent.duration.Duration

/** A process that yields a value of type A if/when it terminates */
sealed abstract class Process[+A] {
  def run(): A
}

/** Send a value of type A over a channel of type C */
case class Out[C <: OutChannel[A], A](c: C, v: A) extends Process[Unit] {
  def map[Y2](f: Unit => Y2): Yielding[Out[C,A], Y2] = impl.YieldingImpl[Out[C,A], Unit, Y2](this, f)
  def flatMap[Y2, P2 <: Process[Y2]](f: Unit => P2): Seq[Out[C,A], P2, Y2] = {
    impl.SeqImpl[Out[C,A], P2, Unit, Y2](this, f)
  }

  override def run(): Unit = {
    println(s"*** Sending ${v} on channel ${c}")
    c.send(v)
  }
}

/** Receive a value of type A from a channel of type C */
case class Recv[C <: InChannel[A], A](c: C)(implicit timeout: Duration) extends Process[A] {
  def map[Y2](f: A => Y2): Yielding[Recv[C, A], Y2] = impl.YieldingImpl[Recv[C,A], A, Y2](this, f)
  def flatMap[B, P2 <: Process[B], F <: A => P2](f: F): In[C, A, F, B] = impl.InImpl(c, f, x => x)

  def apply[B, P2 <: Process[B], F <: A => P2](f: F): In[C, A, F, B] = flatMap(f)

  override def run(): A = {
    println(s"*** Receiving from channel ${c}")
    c.receive()
  }
}

/** Execute a process of type P1, and then a process of type P2 */
abstract class Seq[P1 <: Process[?], P2 <: Process[Y], Y] extends Process[Y]

/* Execute a process of type P and then yield a value of type Y */
abstract class Yielding[P <: Process[?], Y] extends Process[Y] {
  def map[Y2](f2: Y => Y2): Yielding[P, Y2]
}

/** Receive a value of type A from a channel of type C, and continue
  *  as a process of type F.
  */
abstract class In[C <: InChannel[A], A, F <: A => Process[?], Y] extends Process[Y] {
  def map[Y3](f2: Y => Y3): In[C, A, F, Y3]
  def flatMap[Y3, P2 <: Process[Y3]](f: Y => P2): Seq[In[C, A, F, Y], P2, Y3]
}

/** DSL: send v over channel c */
def send[C <: OutChannel[A], A](c: C, v: A) = Out[C, A](c, v)

/** DSL: receive a value from channel c */
def receive[C <: InChannel[A], A](c: C)(implicit timeout: Duration) = Recv[C, A](c)

package impl {
  case class InImpl[C <: InChannel[A], A, F <: A => Process[Y1], Y1, Y2](c: C, andThen: F, mapf: Y1 => Y2)
                                                                        (implicit timeout: Duration) extends In[C, A, F, Y2] {
    def map[Y3](f2: Y2 => Y3): In[C, A, F, Y3] = InImpl[C, A, F, Y1, Y3](c, andThen, x => f2(mapf(x)))

    def flatMap[Y3, P2 <: Process[Y3]](f: Y2 => P2): Seq[In[C, A, F, Y2], P2, Y3] = {
      SeqImpl[In[C, A, F, Y2], P2, Y2, Y3](this, f)
    }

    override def run(): Y2 = {
      println(s"*** In - step 1: receive from ${c}")
      val v = c.receive()
      val cont = andThen(v)
      println(s"*** In - step 2: running continuation ${cont}")
      mapf(cont.run())
    }
  }

  case class YieldingImpl[P <: Process[Y], Y, Y2](process: P, f: Y => Y2) extends Yielding[P, Y2] {
    def map[Y3](f2: Y2 => Y3): Yielding[P, Y3] = YieldingImpl[P, Y, Y3](process, (x: Y) => f2(f(x)))
    def flatMap[Y3, P2 <: Process[Y3], F <: Y2 => P2](f2: F): Seq[P, P2, Y3] = {
      val f3 = (x: Y) => f2(f(x))
      SeqImpl[P, P2, Y, Y3](process, f3)
    }

    def run(): Y2 = f(process.run())
  }

  case class SeqImpl[P1 <: Process[Y1], P2 <: Process[Y2], Y1, Y2](first: P1, andThen: Y1 => P2) extends Seq[P1, P2, Y2] {
    override def run(): Y2 = {
      println(s"*** Seq - step 1: running ${first}")
      val ret = first.run()
      val cont = andThen(ret)
      println(s"*** Seq - step 2: running continuation ${cont}")
      cont.run()
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
// Examples
///////////////////////////////////////////////////////////////////////////////

type Protocol0[C <: Channel[Int]] =
  Yielding[Recv[C, Int], Int]

def test0(c: Channel[Int])(implicit timeout: Duration): Protocol0[c.type] = for {
  v <- receive(c)
} yield v

type Protocol1[C <: Channel[Int]] =
  In[C, Int, (a: Int) =>
    In[C, Int, (b: Int) =>
      Yielding[Out[C, a.type], a.type],
    Int],
  Int]

def test1(c: Channel[Int])(implicit timeout: Duration): Protocol1[c.type] = for {
  v <- receive(c)
  v2 <- receive(c)
  _ <- send(c, v)
} yield v

def test1Approx(c: Channel[Int])(implicit timeout: Duration): Process[Int] = {
  test1(c)
}

type Protocol1B[C <: Channel[Int]] = Protocol1[C]

def test1b(c: Channel[Int])(implicit timeout: Duration): Protocol1[c.type] = for {
  v <- test1(c)
} yield v

type Protocol2[C1 <: Channel[Int], C2 <: OutChannel[Unit], C3 <: OutChannel[Unit]] =
  In[C1, Int, (a: Int) =>
    In[C1, Int, (b: Int) =>
      Seq[Out[C1, a.type],
          Seq[Out[C2, Unit],
              Yielding[Out[C3, Unit], (String, a.type)],
          (String, Int)],
      (String, Int)],
    (String, Int)],
  (String, Int)]

def test2(c: Channel[Int], d: Channel[Unit], e: Channel[Unit])
         (implicit timeout: Duration): Protocol2[c.type, d.type, e.type] = for {
  a <- receive(c)
  b <- receive(c)
  x <- send(c, a)
  _ <- send(d, ())
  z <- send(e, ())
} yield ("Hello", a)

def test3(c: Channel[Int], d: Channel[Unit], e: Channel[Unit])
         (implicit timeout: Duration): Process[(String, Int)] = for {
  x <- test1(c)
  _ <- send(d, ())
  z <- send(e, ())
} yield ("Hello", x)

case class A();
case class B();

type Protocol3[C <: Channel[A|B]] =
  Seq[Out[C,A], Protocol3Recv[C],
      A | B | (String, B)]

type Protocol3Recv[C <: Channel[A|B]] =
  In[C, A|B, (v: A|B) => Protocol3Match[C, v.type],
     A | B | (String, B)]

type Protocol3Match[C <: Channel[A|B], X <: A|B] <: Process[A | B | (String, B)] = X match {
  case A => Yielding[Out[C, X], X]
  case B => Seq[Out[C, X],
                Yielding[Out[C, X], (String, B)],
                (String, B)]
}

type Protocol3Match2[C <: Channel[A|B], X <: A|B] <: Process[A | B | (String, B)] = X match {
  case A => Yielding[Out[C, X], X]
  case B => Seq[Out[C, X],
                Yielding[Out[C, X], (String, B)],
                (String, B)]
}

def test3(c: Channel[A|B])(implicit timeout: Duration): Protocol3[c.type] = for{
  _ <- send(c, A())
  ret <- test3recv(c)
} yield ret

def test3recv(c: Channel[A|B])(implicit timeout: Duration): Protocol3Recv[c.type] = {
  receive(c)(timeout) { x => x match {
    case _: A => for { _ <- send(c, x) } yield x
    case y: B => for {
      _ <- send(c, x)
      _ <- send(c, x)
    } yield ("Hello", y)
  } }
}

def test3MatchB(c: OutChannel[A|B], x: A|B): Seq[Out[c.type, x.type], Yielding[Out[c.type, x.type], x.type], x.type] = for {
  _ <- send(c, x)
  _ <- send(c, x)
} yield x

import scala.compiletime.ops.int.{S,+,-}

type Protocol4[C <: OutChannel[A|B], N <: Int] <: Process[Int] = N match {
  case 0 => Yielding[Out[C, A], N]
  case S[?] => Seq[Out[C, B], Protocol4[C, N - 1],
                       Int]
}

/*
def test4(c: OutChannel[A|B], n: Int): Protocol4[c.type, n.type] = n match {
  case _: 0 => for { _ <- send(c, A()) } yield n
  case n: S[?] => for {
    _ <- send(c, A())
    ret <- test4(c, n-1)
  } yield ret
}
*/

def test4(c: OutChannel[A|B]): Protocol4[c.type, 5] = for {
  _ <- send(c, B())
  _ <- send(c, B())
  _ <- send(c, B())
  _ <- send(c, B())
  _ <- send(c, B())
  _ <- send(c, A())
} yield 0

def main(): Unit = {
  implicit val timeout = Duration("5 seconds")

  val c1 = Channel[Int]()
  val c2 = Channel[Unit]()
  val c3 = Channel[Unit]()

  c1.send(42)
  c1.send(43)
  val p = test3(c1, c2, c3)
  val res = p.run()
  println(s"Result: ${res}")

  val c4 = Channel[A|B]()

  c4.send(B())
  val res2 = test3(c4).run()
  println(s"Result: ${res2}")
}
