// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.channel

import java.util.concurrent.{LinkedTransferQueue => LTQueue}
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration.Duration
import effpi.process.{ProcVar, Process, In}

object ChannelStatus {
  val unscheduled = 0
  val scheduled = 1
  val running = 2
}

/** A channel that can only be used to receive values of type `A`. */
trait InChannel[+A] {
  val in: InChannel[A] = this

  /** Is this channel synchronous? */
  val synchronous: Boolean

  /** Receive a value (waiting up to `timeout`), and return it.
  *
  * @throws RuntimeException if `timeout` expires.
  */
  def receive()(implicit timeout: Duration): A

  /** Return a value if it is immediately available, or `None` otherwise. */
  def poll(): Option[A]

  protected[effpi] val schedulingStatus: AtomicInteger = new AtomicInteger(ChannelStatus.unscheduled)
  protected[effpi] def enqueue(i: (Map[ProcVar[_], (_) => Process], List[() => Process], In[InChannel[Any], Any, Any => Process])): Unit
  protected[effpi] def dequeue(): Option[(Map[ProcVar[_], (_) => Process], List[() => Process], In[InChannel[Any], Any, Any => Process])]
}

/** A channel that can only be used to send values of type `A`. */
trait OutChannel[-A] {
  val out: OutChannel[A] = this

  /* The "dual" input channel that reads data from this output channel */
  protected [effpi] val dualIn: InChannel[Any]

  /** Is this channel synchronous? */
  val synchronous: Boolean

  /** Send value `v` through the channel. */
  def send(v: A): Unit
}

/** A channel that can be used to send and receive values of type `A`. */
abstract class Channel[A] extends InChannel[A] with OutChannel[A] {
  /** Is this channel synchronous? */
  override val synchronous: Boolean
}

object Channel {
  /** Return a synchronous channel. */
  def apply[A](): Channel[A] = QueueChannel.pipe[A](true)
}

trait QueueInChannel[+A](q: LTQueue[A]) extends InChannel[A] {

  var pendingInProcesses = new LTQueue[(Map[ProcVar[_], (_) => Process], List[() => Process], In[InChannel[Any], Any, Any => Process])]()

  override def receive()(implicit timeout: Duration) = {
    if (!timeout.isFinite) {
      q.take()
    } else {
      val ret = q.poll(timeout.length, timeout.unit)
      if (ret == null) {
        throw new RuntimeException(s"${this}: timeout after ${timeout}")
      }
      ret
    }
  }

  override def poll() = q.poll() match {
    case null => None
    case head => Some(head)
  }

  override def enqueue(i: (Map[ProcVar[_], (_) => Process], List[() => Process], In[InChannel[Any], Any, Any => Process])): Unit = pendingInProcesses.add(i)

  override def dequeue() = pendingInProcesses.poll() match {
    case null => None
    case head => Some(head)
  }

}

trait QueueOutChannel[-A](q: LTQueue[A])
                         (maybeDual: Option[QueueInChannel[Any]]) extends OutChannel[A] {
  override def send(v: A) = q.add(v)

  override val dualIn: InChannel[Any] = maybeDual match {
    case None => new QueueInChannel(q) {
      override val synchronous: Boolean = this.synchronous
    }
    case Some(d) => d
  }
}

//TODO: may not want to pass None here for the outqueue
class QueueChannel[A](q1: LTQueue[A], q2: LTQueue[A],
                      override val synchronous: Boolean)
                     (maybeDual: Option[QueueChannel[A]])
  extends Channel[A] with QueueInChannel(q1)
                     with QueueOutChannel(q2)(None) {
}

object QueueChannel {
  def pipe[A]: Pipe[A] = pipe[A](false)
  
  def pipe[A](synchronous: Boolean): Pipe[A] = {
    val q = new LTQueue[A]
    val in = new QueueInChannel(q) {
      override val synchronous: Boolean = synchronous
    }
    val out = new QueueOutChannel(q)(Some(in)) {
      override val synchronous: Boolean = synchronous
    }
    new PipeImpl[A](in, out)
  }

  def apply[A](synchronous: Boolean): QueueChannel[A] = {
    apply(LTQueue[A](), LTQueue[A](), synchronous)
  }

  def apply[A](q1: LTQueue[A], q2: LTQueue[A], synchronous: Boolean) = {
    new QueueChannel(q1, q2, synchronous)(None)
  }
}

abstract class Pipe[A](override val in: InChannel[A],
                       override val out: OutChannel[A])
  extends Channel[A] {
  // Members declared in InChannel
  override def receive()(implicit timeout: Duration) = in.receive()(timeout)
  override def dequeue() = in.dequeue()
  override def enqueue(i: (Map[ProcVar[_], (_) => Process], List[() => Process], In[InChannel[Any], Any, Any => Process])) = in.enqueue(i)
  override def poll() = in.poll()

  // Members declared in OutChannel
  override def send(v: A) = out.send(v)
  override val dualIn = out.dualIn
}

private class PipeImpl[A](in: InChannel[A], out: OutChannel[A])
  extends Pipe[A](in, out) {
    assert(in.synchronous == out.synchronous)
    override val synchronous: Boolean = in.synchronous
  }
