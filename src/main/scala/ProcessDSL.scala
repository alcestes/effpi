// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.process

import effpi.channel._
import effpi.system._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

// WARNING: double-check variance
sealed abstract class Process {
  def >>[P1 >: this.type <: Process, P2 <: Process](cont: => P2) = >>:[P1, P2](() => this, () => cont)

  def fork[P1 >: this.type <: Process] = Fork[P1](() => this)

  def spawn(ps: ProcessSystem) = {
    val self = this
    ps.scheduleProc((Map(), Nil, self))
  }
}
case class Out[C <: OutChannel[A], A](channel: C, v: A) extends Process

// FIXME:
// * If we make In contravariant on A, type inference on "cont"
//   can push the domain to any, and therefore, we lose pattern matching
//   exhaustiveness checks --- e.g., on test2() below
//
// * If we make In invariant on A, we restore pattern matching exhaustiveness
//   checks on test2() below; however, we restrict subtyping, and we
//   sometimes need type annotations; see e.g. "cont" in test1a() and test1c()
//
// * If we make In covariant on A, we are wrong (and besides, we get a
//   variance error in the definition of In)
/** Receive a value from `channel`, and pass it to `cont`. */
case class In[C <: InChannel[A], A, P <: A => Process](channel: C, cont: P, timeout: Duration) extends Process

case class Fork[P <: Process](p: () => P) extends Process

sealed abstract class PNil() extends Process

abstract class YieldCtx[-A](protected[effpi] val chan: OutChannel[A])
protected[effpi] class YieldCtxImpl[-A](chan: OutChannel[A]) extends YieldCtx[A](chan)
case class Yield[A](v: A)(implicit val ctx: Option[YieldCtx[A]]) extends Process

abstract class ProcVar[A](name: String)
case class ProcX[A]() extends ProcVar[A]("X")
case class ProcY[A]() extends ProcVar[A]("Y")
case class ProcZ[A]() extends ProcVar[A]("Z")

abstract class RecVar[A](name: String) extends ProcVar[A](name)

sealed abstract class RecX[A]() extends RecVar[A]("X")
case object RecX extends RecX[Unit]

sealed abstract class RecY[A]() extends RecVar[A]("Y")
case object RecY extends RecY[Unit]

sealed abstract class RecZ[A]() extends RecVar[A]("Z")
case object RecZ extends RecZ[Unit]

case class Def[V <: ProcVar, A, P1 <: Process, P2 <: Process](name: V[A], pdef: A => P1, in: () => P2) extends Process
case class Call[V <: ProcVar, A](procvar: V[A], arg: A) extends Process

case class >>:[P1 <: Process, P2 <: Process](p1: () => P1, p2: () => P2) extends Process

package object dsl {
  /** Recursion: `P` loops on `V`, that represent a bound recursion variable. */
  type Rec[V <: RecVar, P <: Process] = Def[V, Unit, P, P]

  /** Loop on a recursion variable `V`, expected to be bound by [[Rec]].*/
  type Loop[V <: RecVar] = Call[V, Unit]

  /** Execute `P1` and `P2` in parallel. */
  type Par[P1 <: Process, P2 <: Process] = Fork[P1] >>: P2

  /** Execute three processes in parallel. */
  type Par3[P1 <: Process, P2 <: Process, P3 <: Process] = Par[P1, Par[P2, P3]]

  /** Execute four processes in parallel. */
  type Par4[P1 <: Process, P2 <: Process, P3 <: Process, P4 <: Process] = (
    Par[P1, Par3[P2, P3, P4]]
  )

  /** Execute five processes in parallel. */
  type Par5[P1 <: Process, P2 <: Process, P3 <: Process, P4 <: Process,
            P5 <: Process] = (
    Par[P1, Par4[P2, P3, P4, P5]]
  )

  /** Execute six processes in parallel. */
  type Par6[P1 <: Process, P2 <: Process, P3 <: Process, P4 <: Process,
            P5 <: Process, P6 <: Process] = (
    Par[P1, Par5[P2, P3, P4, P5, P6]]
  )

  /** Execute seven processes in parallel. */
  type Par7[P1 <: Process, P2 <: Process, P3 <: Process, P4 <: Process,
            P5 <: Process, P6 <: Process, P7 <: Process] = (
    Par[P1, Par6[P2, P3, P4, P5, P6, P7]]
  )

  /** Execute eight processes in parallel. */
  type Par8[P1 <: Process, P2 <: Process, P3 <: Process, P4 <: Process,
            P5 <: Process, P6 <: Process, P7 <: Process, P8 <: Process] = (
    Par[P1, Par7[P2, P3, P4, P5, P6, P7, P8]]
  )

  /** Execute nine processes in parallel. */
  type Par9[P1 <: Process, P2 <: Process, P3 <: Process, P4 <: Process,
            P5 <: Process, P6 <: Process, P7 <: Process, P8 <: Process,
            P9 <: Process] = (
    Par[P1, Par8[P2, P3, P4, P5, P6, P7, P8, P9]]
  )

  /** Execute ten processes in parallel. */
  type Par10[P1 <: Process, P2 <: Process, P3 <: Process, P4 <: Process,
             P5 <: Process, P6 <: Process, P7 <: Process, P8 <: Process,
             P9 <: Process, P10 <: Process] = (
    Par[P1, Par9[P2, P3, P4, P5, P6, P7, P8, P9, P10]]
  )

  /** Execute 11 processes in parallel. */
  type Par11[P1 <: Process, P2 <: Process,  P3 <: Process, P4 <: Process,
             P5 <: Process, P6 <: Process,  P7 <: Process, P8 <: Process,
             P9 <: Process, P10 <: Process, P11 <: Process] = (
    Par[P1, Par10[P2, P3, P4, P5, P6, P7, P8, P9, P10, P11]]
  )

  /** Execute 12 processes in parallel. */
  type Par12[P1 <: Process, P2 <: Process,  P3 <: Process,  P4 <: Process,
             P5 <: Process, P6 <: Process,  P7 <: Process,  P8 <: Process,
             P9 <: Process, P10 <: Process, P11 <: Process, P12 <: Process] = (
    Par[P1, Par11[P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12]]
  )

  /** Execute 13 processes in parallel. */
  type Par13[P1 <: Process,  P2 <: Process,  P3 <: Process,  P4 <: Process,
             P5 <: Process,  P6 <: Process,  P7 <: Process,  P8 <: Process,
             P9 <: Process,  P10 <: Process, P11 <: Process, P12 <: Process,
             P13 <: Process] = (
    Par[P1, Par12[P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13]]
  )

  /** Execute 14 processes in parallel. */
  type Par14[P1 <: Process,  P2 <: Process,  P3 <: Process,  P4 <: Process,
             P5 <: Process,  P6 <: Process,  P7 <: Process,  P8 <: Process,
             P9 <: Process,  P10 <: Process, P11 <: Process, P12 <: Process,
             P13 <: Process, P14 <: Process] = (
    Par[P1, Par13[P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14]]
  )

  /** Execute `P` so that it can yield a value of type `A` to the caller.
  *
  * This is an experimental type, with the goal of modelling the
  * [[https://doc.akka.io/docs/akka/2.5/actors.html#ask-send-and-receive-future "ask pattern"]]. */
  type Yielding[A, P <: Process] = given YieldCtx[A] => P

  /** Do nothing (inactive process). */
  case object nil extends PNil

  /** Send argument `v` via channel `c`. */
  def send[C <: OutChannel[A], A](c: C, v: A) = Out[C,A](c, v)

  /** Use channel `c` to receive a value, then pass it to the `cont`inuation. */
  def receive[C <: InChannel[A], A, P <: A => Process](c: C)(cont: P)(implicit timeout: Duration) = In[C,A,P](c, cont, timeout)

  /** Fork `p` as a separate process.
  *
  * NOTE: in practice, you might probably want to use [[par]].
  */
  def fork[P <: Process](p: => P) = Fork[P](() => p)

  /** Execute two processes in parallel. */
  def par[P1 <: Process,
          P2 <: Process](p1: => P1, p2: => P2): Par[P1, P2] = {
    Fork[P1](() => p1) >> p2
  }

  /** Execute three processes in parallel. */
  def par[P1 <: Process,
          P2 <: Process,
          P3 <: Process](p1: => P1,
                         p2: => P2,
                         p3: => P3): Par3[P1, P2, P3] = {
    par(p1, par(p2, p3))
  }

  /** Execute four processes in parallel. */
  def par[P1 <: Process,
          P2 <: Process,
          P3 <: Process,
          P4 <: Process](p1: => P1,
                         p2: => P2,
                         p3: => P3,
                         p4: => P4): Par4[P1, P2, P3, P4] = {
    par(p1, par(p2, p3, p4))
  }

  /** Execute five processes in parallel. */
  def par[P1 <: Process,
          P2 <: Process,
          P3 <: Process,
          P4 <: Process,
          P5 <: Process](p1: => P1,
                         p2: => P2,
                         p3: => P3,
                         p4: => P4,
                         p5: => P5): Par5[P1, P2, P3, P4, P5] = {
    par(p1, par(p2, p3, p4, p5))
  }

  /** Execute six processes in parallel. */
  def par[P1 <: Process,
          P2 <: Process,
          P3 <: Process,
          P4 <: Process,
          P5 <: Process,
          P6 <: Process](p1: => P1,
                         p2: => P2,
                         p3: => P3,
                         p4: => P4,
                         p5: => P5,
                         p6: => P6): Par6[P1, P2, P3, P4, P5, P6] = {
    par(p1, par(p2, p3, p4, p5, p6))
  }

  /** Execute seven processes in parallel. */
  def par[P1 <: Process,
          P2 <: Process,
          P3 <: Process,
          P4 <: Process,
          P5 <: Process,
          P6 <: Process,
          P7 <: Process](p1: => P1,
                         p2: => P2,
                         p3: => P3,
                         p4: => P4,
                         p5: => P5,
                         p6: => P6,
                         p7: => P7): Par7[P1, P2, P3, P4, P5, P6, P7] = {
    par(p1, par(p2, p3, p4, p5, p6, p7))
  }

  /** Execute eight processes in parallel. */
  def par[P1 <: Process,
          P2 <: Process,
          P3 <: Process,
          P4 <: Process,
          P5 <: Process,
          P6 <: Process,
          P7 <: Process,
          P8 <: Process](p1: => P1,
                         p2: => P2,
                         p3: => P3,
                         p4: => P4,
                         p5: => P5,
                         p6: => P6,
                         p7: => P7,
                         p8: => P8): Par8[P1, P2, P3, P4, P5, P6, P7, P8] = {
    par(p1, par(p2, p3, p4, p5, p6, p7, p8))
  }

  /** Yield a value `v` to the caller of the present process.
  *
  * This is an experimental method, that can only be invoked within a process
  * typed by [[Yielding]]. Its goal is to capture the
  * [[https://doc.akka.io/docs/akka/2.5/actors.html#ask-send-and-receive-future "ask pattern"]]. */
  def pyield[A](v: A): given YieldCtx[A] => Yield[A] = {
    Yield[A](v)(Some(implicitly[YieldCtx[A]]))
  }

  def pdef[V <: ProcVar, A, P1 <: Process, P2 <: Process](name: V[A])(pdef: A => P1)(in: => P2) = Def[V, A, P1, P2](name, pdef, () => in)

  def pcall[V <: ProcVar, A](name: V[A], arg: A) = Call[V,A](name, arg)

  def rec[V <: RecVar, P <: Process](v: V[Unit])(p: => P): Rec[V, P] = Def[V, Unit, P, P](v, (x) => p, () => p)

  def loop[V <: RecVar](v: V[Unit]): Loop[V] = Call[V, Unit](v, ())

  def eval(p: Process): Try[Unit] = Try(eval(Map(), Nil, p))

  @annotation.tailrec
  def eval(env: Map[ProcVar[_], (_) => Process], lp: List[() => Process],
           p: Process): Unit = p match {
    case i: In[_,_,_] => {
      val v: Any = i.channel.receive()(i.timeout)
      if (i.channel.synchronous) {
        // We received a tuple containing a value and an ack channel
        val (v2, ack) = v.asInstanceOf[Tuple2[Any, OutChannel[Unit]]]
        ack.send(())
        eval(env, lp, i.cont.asInstanceOf[Any => Process](v2))
      } else {
        eval(env, lp, i.cont.asInstanceOf[Any => Process](v))
      }
    }
    case o: Out[_,_] => {
      val oc = o.channel.asInstanceOf[OutChannel[Any]]
      if (oc.synchronous) {
        // Send an ack channel together with the value
        val ack = QueueChannel.apply[Unit]()
        oc.send((o.v, ack.out))
        // FIXME: allow to specify timeouts
        ack.in.receive()(concurrent.duration.Duration.Inf)
      } else {
        oc.send(o.v)
      }
      lp match {
        case Nil => ()
        case lh :: lt => eval(env, lt, lh())
      }
    }
    case f: Fork[_] => {
      new Thread { override def run = eval(env, Nil, f.p()) }.start()
      lp match {
        case Nil => ()
        case lh :: lt => eval(env, lt, lh())
      }
    }
    case n: PNil => lp match {
      case Nil => ()
      case lh :: lt => eval(env, lt, lh())
    }
    case y: Yield[_] => {
      y.ctx match {
        case Some(c) => c.chan.asInstanceOf[OutChannel[Any]].send(y.v)
        case None => ()
      }
      lp match {
        case Nil => ()
        case lh :: lt => eval(env, lt, lh())
      }
    }
    case d: Def[_,_,_,_] => {
      eval(env + (d.name -> d.pdef), lp, d.in())
    }
    case c: Call[_,_] => {
      env.get(c.procvar) match {
        case Some(p) => {
          // println(s"*** Calling ${c.procvar}(${c.arg})")
          eval(env, lp, p.asInstanceOf[Any => Process](c.arg))
        }
        case None => {
          throw new RuntimeException(s"Unbound process variable: ${c.procvar}")
        }
      }
    }
    case s: >>:[_,_] => {
      eval(env, s.p2 :: lp, s.p1())
    }
  }
}
