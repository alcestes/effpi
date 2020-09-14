// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.actor

import effpi.channel._
import effpi.process._
import effpi.system._

import scala.util.{Failure, Success, Try}

package object dsl {
  import effpi.process.{dsl => pdsl}
  import scala.concurrent.duration.Duration

  type Spawn[P1 <: Process, P2 <: Process] = Fork[P1] >>: P2

  /** Receive a value of type T on its mailbox, with continuation of type P. */
  type ReadDep[T, P <: T => Process] = In[Mailbox[T], T, P]
  type Read[T, P <: Process] = ReadDep[T, T => P]
  type SendTo[R <: ActorRef[T], T] = Out[R, T]
  type Send[T] = SendTo[ActorRef[T], T]
  type Ask[R <: ActorRef[Req], Req, Resp, P <: Process] =
         (SendTo[R, Req] >>: ReadDep[Resp, (x: Resp) => Yield[x.type]]) >>: P

  type Actor[A, T] = ActorCtx[A] ?=> T

  class Behavior[A, P](body: => Actor[A, P])
        extends Function0[Actor[A, P]] {
    def apply() = body
  }

  object Behavior {
    def apply[A, P](body: => Actor[A,P]) = {
      new Behavior[A,P](body)
    }
  }

  object Actor {
    def spawn[A, P <: Process](beh: Behavior[A, P])(implicit ps: ProcessSystem): ActorRef[A] = {
      val chan = ActorChannel.async[A]()
      implicit val ctx = new ActorCtxImpl[A](chan.ref, chan.mbox)
      val proc = beh()
      proc.spawn(ps)
      chan.ref
    }
  }

  sealed abstract class RecAt[A] extends RecVar[A]("InfiniteActorLoop")
  case object RecA extends RecAt[Unit]
  case object RecB extends RecAt[Unit]
  type Forever[P <: Process] = pdsl.Rec[RecAt, P >>: pdsl.Loop[RecAt]]

  /** Use the mailbox `mbox` from the actor context obtained implicitly to
   *  receive a value, then pass it to the `cont`inuation. */
  def read[T, P <: Process, TP <: T => P](cont: TP)
                                         (implicit timeout: Duration): Actor[T, ReadDep[T, TP]] = {
    pdsl.receive[Mailbox[T], T, TP](implicitly[ActorCtx[T]].mbox)(cont)(timeout)
  }

  /** Send argument `x` to an actor via its reference `ref`. */
  def send[R <: ActorRef[T], T](ref: R, x: T): SendTo[R, T] = pdsl.send(ref, x)

  def call[T, P <: Process](p: () => Actor[T, P])
                           (implicit timeout: Duration): P = {
    val chan = ActorChannel.async[T]()
    implicit val ctx = new ActorCtxImpl[T](chan.ref, chan.mbox)
    p()
  }
  def call[T, Y, P1 <: Process, P2 <: Process](p: () => Actor[T, pdsl.Yielding[Y, P1]])
                                              (cont: Y => P2)
                                              (implicit timeout: Duration): P1 >>: P2 = {
    val ychan = Channel.async[Y]()
    implicit val yctx = new YieldCtxImpl(ychan.out)
    val chan = ActorChannel.async[T]()
    implicit val ctx = new ActorCtxImpl[T](chan.ref, chan.mbox)
    p() >> cont(ychan.in.receive())
  }

  private def askBeh[Req, Resp]
                    (srv: ActorRef[Req], query: ActorRef[Resp] => Req)
                    (implicit timeout: Duration) = Behavior[Resp,
             pdsl.Yielding[Resp, SendTo[srv.type, Req] >>: ReadDep[Resp, (x:Resp) => Yield[x.type]]]] {
    send(srv, query(self)) >>
    read { x =>
      pdsl.pyield(x)
    }
  }
  def ask[Req, Resp, P <: Process]
         (srv: ActorRef[Req])
         (query: ActorRef[Resp] => Req)
         (cont: Resp => P)
         (implicit timeout: Duration): Ask[srv.type, Req, Resp, P] = {
    call(askBeh(srv, query))(cont)
  }

  def self[T]: Actor[T, ActorRef[T]] = {
    implicitly[ActorCtx[T]].self
  }

  /** Spawn a new actor passing it a fresh context, comprising a new reference
   *  `ref` and a new mailbox `mbox`. It then passes the reference to the
   *  `cont`inuation*/
  def spawn[T, P1 <: Process, P2 <: Process]
           (beh: Behavior[T, P1])
           (cont: ActorRef[T] => P2): Spawn[P1, P2] = {
    val chan = ActorChannel.async[T]()
    pdsl.fork {
      // Here we are inlining an actor; we could use pdsl.spawn, but
      // pfork is already running the following behaviour in parallel
      implicit val ctx = new ActorCtxImpl[T](chan.ref, chan.mbox)
      beh()
    } >> cont(chan.ref)
  }

  def forever[P <: Process](beh: => P): Forever[P] = {
    // FIXME: here we should create a RecVar with a random name
    pdsl.rec(RecA)(beh >> pdsl.loop(RecA))
  }

  /** Do nothing (inactive actor). */
  def nil: PNil = pdsl.nil
}
