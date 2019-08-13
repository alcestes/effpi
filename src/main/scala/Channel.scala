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

  /** Return a new channel with the same characteristics (e.g., same message
    * transport, synchronous or asynchronous,...). */
  def create[B](): Channel[B] = create[B](this.synchronous)

  /** Return a new channel with the same characteristics (e.g., same message
    * transport,...), and the given synchronous/asynchronpus behaviour. */
  def create[B](synch: Boolean): Channel[B]
}

/** A channel that can be used to send and receive values of type `A`. */
abstract class Channel[A] extends InChannel[A] with OutChannel[A] {
  /** Is this channel synchronous? */
  override val synchronous: Boolean

  // Methods declared in InChannel
  override def receive()(implicit timeout: Duration) = in.receive()(timeout)
  override def dequeue() = in.dequeue()
  override def enqueue(i: (Map[ProcVar[_], (_) => Process], List[() => Process], In[InChannel[Any], Any, Any => Process])) = in.enqueue(i)
  override def poll() = in.poll()

  // Methods declared in OutChannel
  override def send(v: A) = out.send(v)
  override val dualIn = out.dualIn
  override def create[B](synch: Boolean): Channel[B] = {
    out.create[B](synch)
  }
}

object Channel {
  /** Return a synchronous channel. */
  def apply[A](): Channel[A] = QueueChannel.apply[A]()

  /** Return an asynchronous channel: what is written on the `out` endpoint
    * can be received from the `in` endpoint. */
  def async[A](): Channel[A] = QueueChannel.async[A]()

  /** Return two paired asynchronous channels: what is sent on one can be
    * received from the other. */
  def pair[A](): (Channel[A], Channel[A]) = QueueChannel.pair[A]()
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

  override def create[B](synch: Boolean): QueueChannel[B] = {
    QueueChannel.apply(synch)
  }
}

//TODO: may not want to pass None here for the outqueue
class QueueChannel[A](override val in: QueueInChannel[A],
                      override val out: QueueOutChannel[A])
  extends Channel[A] {
  assert(in.synchronous == out.synchronous)
  override val synchronous = in.synchronous
}

object QueueChannel {
  /** Return a synchronous queue-based channel. */
  def apply[A](): QueueChannel[A] = apply[A](true)

  /** Return an asynchronous queue-based channel. */
  def async[A](): QueueChannel[A] = apply[A](false)
  
  /** Return a synchronous or asynchronous queue-based channel. */
  def apply[A](synch: Boolean): QueueChannel[A] = {
    val q = LTQueue[A]()
    val in = new QueueInChannel(q) {
      override val synchronous: Boolean = synch
    }
    val out = new QueueOutChannel(q)(Some(in)) {
      override val synchronous: Boolean = synch
    }
    new QueueChannel[A](in, out)
  }

  /** Return two paired asynchronous queue-based channels: what is sent on one
    * can be received from the other. */
  def pair[A](): (QueueChannel[A], QueueChannel[A]) = {
    val q1 = LTQueue[A]()
    val q2 = LTQueue[A]()
    val in1 = new QueueInChannel(q1) {
      override val synchronous: Boolean = false
    }
    val out1 = new QueueOutChannel(q1)(Some(in1)) {
      override val synchronous: Boolean = false
    }
    val in2 = new QueueInChannel(q2) {
      override val synchronous: Boolean = false
    }
    val out2 = new QueueOutChannel(q2)(Some(in2)) {
      override val synchronous: Boolean = false
    }

    (new QueueChannel[A](in1, out2), new QueueChannel[A](in2, out1))
  }
}
