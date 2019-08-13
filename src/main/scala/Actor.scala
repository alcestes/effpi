// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.actor

import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.{Future, Promise, Await}

import effpi.channel.{Channel, InChannel, OutChannel, QueueChannel}
import effpi.process.{ProcVar, Process, In}
import effpi.system._
import scala.concurrent.duration.Duration

abstract class Mailbox[+A] extends InChannel[A]

private class MailboxImpl[A](c: InChannel[A]) extends Mailbox[A] {
  override val synchronous: Boolean = c.synchronous
  
  override def receive()(implicit timeout: Duration) = c.receive()(timeout)

  override def poll() = c.poll()

  override def enqueue(i: (Map[ProcVar[_], (_) => Process],
                           List[() => Process],
                           In[InChannel[Any], Any, Any => Process])) = c.enqueue(i)

  override def dequeue() = c.dequeue()
}

abstract class ActorRef[-A] extends OutChannel[A] {
  def ! = send
}

private class ActorRefImpl[A](c: OutChannel[A])
                             (maybeDual: Option[Mailbox[Any]]) extends ActorRef[A] {
  override val synchronous: Boolean = c.synchronous

  override def send(v: A) = c.send(v)

  override val dualIn: Mailbox[Any] = maybeDual match {
    case None => new MailboxImpl(c.dualIn)
    case Some(d) => d
  }
}

protected[actor] abstract class ActorChannel[A] extends Channel[A] {
  val mbox: Mailbox[A]
  val ref: ActorRef[A]
}

private class ActorChannelImpl[A](override val mbox: Mailbox[A],
                                  override val ref: ActorRef[A])
  extends ActorChannel[A] {
    assert(mbox.synchronous == ref.synchronous)
    override val synchronous: Boolean = mbox.synchronous
}

object ActorChannel {
  /** Create a synchronous `ActorChannel`. */
  def apply[A](): ActorChannel[A] = apply(true)
  
  /** Create an asynchronous `ActorChannel`. */
  def async[A](): ActorChannel[A] = apply(false)
  
  def apply[A](synchronous: Boolean): ActorChannel[A] = {
    val p = QueueChannel.apply[A](synchronous)
    val mbox = new MailboxImpl(p.in)
    val ref = new ActorRefImpl(p.out)(Some(mbox))
    new ActorChannelImpl(mbox, ref)
  }
}

abstract class ActorCtx[A](val self: ActorRef[A],
                           protected[actor] val mbox: Mailbox[A])

protected[actor] class ActorCtxImpl[A](self: ActorRef[A],
                                       mbox: Mailbox[A]) extends ActorCtx[A](self, mbox)

object patterns {
  /** Implements the ask pattern, to interact with an actor `srv` from
    * "outside" its [[ProcessSystem]] `ps`: send a `query`, and wait for
    * a response of type `Resp`. */
  def ask[Req, Resp](srv: ActorRef[Req], query: ActorRef[Resp] => Req)
                    (implicit ps: ProcessSystem, timeout: Duration): Resp = {
    import effpi.process.{dsl => pdsl}

    val respPromise = Promise[Resp]()
    val respFuture = respPromise.future

    val pipe = ActorChannel[Resp]()
    val askProcess = {
      pdsl.send(srv, query(pipe.ref)) >>
      pdsl.receive(pipe.mbox) { msg: Resp =>
        respPromise.success(msg)
        pdsl.nil
      }
    }
    askProcess.spawn(ps)
    Await.result(respFuture, Duration.Inf)
  }
}
