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

  /** Receive a value (waiting up to `timeout`), and return it.
  *
  * @throws RuntimeException if `timeout` expires.
  */
  def receive()(implicit timeout: Duration): A

  /** Provide an asynchronous view of this channel.
  * NOTE: this method is internal, and may throw an exception if unsupported
  */
  protected[effpi] def async: AsyncInChannel[A]
}

/** A channel that can only be used to send values of type `A`. */
trait OutChannel[-A] {
  val out: OutChannel[A] = this

  /** Send value `v` through the channel. */
  def send(v: A): Unit

  /** Provide an asynchronous view of this channel.
  * NOTE: this method is internal, and may throw an exception if unsupported
  */
  protected[effpi] def async: AsyncOutChannel[A]
}

/** A channel that can be used to send and receive values of type `A`. */
abstract class Channel[T] extends InChannel[T] with OutChannel[T] {
  // FIXME: make protected, usable by Connection
  protected[effpi] def dual: Channel[T]

  /** Provide an asynchronous view of this channel.
  * NOTE: this method is internal, and may throw an exception if unsupported
  */
  protected[effpi]
  override def async: AsyncInChannel[T] & AsyncOutChannel[T]
}

private class SyncChannel[A] extends Channel[A] {
  import scala.concurrent.{Await, Promise, Future}
  import scala.util.{Success, Failure}
  // A queue of values, and Promises used to notify reception
  private val values = new LTQueue[(A, Promise[Unit])]()

  override val dual = this

  override def receive()(implicit timeout: Duration) = {
    val recv = if (!timeout.isFinite) {
      values.take()
    } else {
      val ret = values.poll(timeout.length, timeout.unit)
      if (ret == null) {
        throw new RuntimeException(s"${this}: timeout after ${timeout}")
      }
      ret
    }
    recv._2.complete(Success(()))
    recv._1
  }

  override def send(v: A): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val notify = Promise[Unit]()
    values.add((v, notify))
    Await.result(notify.future, Duration.Inf) // TODO: allow to set timeut?
  }

  protected[effpi]
  override def async: AsyncInChannel[A] & AsyncOutChannel[A] = {
    throw new UnsupportedOperationException(s"Cannot view a strictly synchronous channel as asynchronous")
  }
}

object Channel {
  def apply[A](): Channel[A] = new SyncChannel()
}

trait AsyncInChannel[+A] extends InChannel[A] {
  override val in: AsyncInChannel[A] = this
  def poll(): Option[A]
  val schedulingStatus: AtomicInteger = new AtomicInteger(ChannelStatus.unscheduled)
  def enqueue(i: (Map[ProcVar[_], (_) => Process], List[() => Process], In[AsyncInChannel[Any], Any, Any => Process])): Unit
  def dequeue(): Option[(Map[ProcVar[_], (_) => Process], List[() => Process], In[AsyncInChannel[Any], Any, Any => Process])]
  protected[effpi] override val async = this
}

trait AsyncOutChannel[-A] extends OutChannel[A] {
  override val out: AsyncOutChannel[A] = this
  val dualIn: AsyncInChannel[Any]
  protected[effpi] override val async = this
}

trait QueueInChannel[+A](q: LTQueue[A]) extends AsyncInChannel[A] {

  var pendingInProcesses = new LTQueue[(Map[ProcVar[_], (_) => Process], List[() => Process], In[AsyncInChannel[Any], Any, Any => Process])]()

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

  override def enqueue(i: (Map[ProcVar[_], (_) => Process], List[() => Process], In[AsyncInChannel[Any], Any, Any => Process])): Unit = pendingInProcesses.add(i)

  override def dequeue() = pendingInProcesses.poll() match {
    case null => None
    case head => Some(head)
  }

}

trait QueueOutChannel[-A](q: LTQueue[A])(maybeDual: Option[QueueInChannel[Any]]) extends AsyncOutChannel[A] {
  override def send(v: A) = q.add(v)

  override val dualIn: AsyncInChannel[Any] = maybeDual match {
    case None => new QueueInChannel(q){}
    case Some(d) => d
  }
}

//TODO: may not want to pass None here for the outqueue
class QueueChannel[T](q1: LTQueue[T], q2: LTQueue[T])(maybeDual: Option[QueueChannel[T]])
  extends Channel[T] with QueueInChannel(q1) with QueueOutChannel(q2)(None) {

  override lazy val dual: QueueChannel[T] = maybeDual match {
    case None => new QueueChannel(q2, q1)(Some(this))
    case Some(d) => d
  }

  protected[effpi] override val async = this
}

object QueueChannel {
  def pipe[A]: AsyncPipe[A] = {
    val q = new LTQueue[A]
    val in = new QueueInChannel(q){}
    val out = new QueueOutChannel(q)(Some(in)){}
    new AsyncPipeImpl[A](in, out)
  }

  def apply[T](q1: LTQueue[T], q2: LTQueue[T]) = new QueueChannel(q1, q2)(None)
}

abstract class Pipe[A](override val in: InChannel[A],
                       override val out: OutChannel[A])
  extends InChannel[A] with OutChannel[A] {
  override def receive()(implicit timeout: Duration) = in.receive()(timeout)

  override def send(v: A) = out.send(v)

  protected[effpi] override val async = {
    throw new UnsupportedOperationException(s"Cannot view a strictly synchronous pipe as asynchronous")
  }
}

abstract class AsyncPipe[A](override val in: AsyncInChannel[A],
                            override val out: AsyncOutChannel[A]) extends AsyncInChannel[A]
                                                     with AsyncOutChannel[A] {
  // Members declared in AsyncInChannel
  override def receive()(implicit timeout: Duration) = in.receive()(timeout)
  override def dequeue() = in.dequeue()
  override def enqueue(i: (Map[ProcVar[_], (_) => Process], List[() => Process], In[AsyncInChannel[Any], Any, Any => Process])) = in.enqueue(i)
  override def poll() = in.poll()

  // Members declared in AsyncOutChannel
  override def send(v: A) = out.send(v)
  override val dualIn = out.dualIn

  protected[effpi] override val async = this
}

private class AsyncPipeImpl[A](in: AsyncInChannel[A], out: AsyncOutChannel[A])
  extends AsyncPipe[A](in, out)
