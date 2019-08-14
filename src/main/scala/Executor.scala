// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.system

import java.lang.Runnable

import scala.util.{Failure, Success, Try}
import scala.concurrent.duration.Duration

import effpi.channel.{InChannel, OutChannel}

protected[system] class Executor(ps: ProcessSystem, stepsLeft: Int = 10) extends Runnable {

  import effpi.process._

  override def run() = {
    while (ps.alive) {
      val head = ps.consumeProc()
      head match {
        case Some(p) =>
          fastEval(p, stepsLeft)
        case None =>
          ()
      }
    }
  }

  @annotation.tailrec
  private def fastEval(
    proc: (Map[ProcVar[_], (_) => Process], List[() => Process], Process),
    stepsLeft: Int
  ): Unit = {
    if (stepsLeft <= 0) {
      ps.scheduleProc(proc)
    } else {
      val (env, lp, p) = proc
      p match {
      case i: In[_,_,_] => {
        ps match {
          case _: ProcessSystemRunnerImproved => {
            i.channel.enqueue((env, lp, i.asInstanceOf[In[InChannel[Any], Any, Any => Process]]))
            ()
          }
          case _: ProcessSystemStateMachineMultiStep => {
            val ic = i.channel
            ic.poll() match {
              case Some(v) =>
                val cont = if (ic.synchronous) {
                  // We received a tuple containing a value and an ack channel
                  val (v2, ack) = v.asInstanceOf[Tuple2[Any, OutChannel[Unit]]]
                  ack.send(())
                  // The ack recipient may need to be woken up
                  // Remember: ps is ProcessSystemStateMachineMultiStep
                  ps.smartEnqueue(ack.dualIn)
                  i.cont.asInstanceOf[Any => Process](v2)
                } else {
                  i.cont.asInstanceOf[Any => Process](v)
                }
                //ps.forceSchedule(ic)
                fastEval((env, lp, cont), stepsLeft - 1)
              case None =>
                ic.enqueue((env, lp, i.asInstanceOf[In[InChannel[Any], Any, Any => Process]]))
                ps.smartEnqueue(ic)
            }
          }
        }
      }
      case o: Out[_,_] => {
        val outCh = o.channel.asInstanceOf[OutChannel[Any]]
        val ack: Option[InChannel[Unit]] = if (outCh.synchronous) {
          // Send an ack channel together with the value
          val ack = outCh.create[Unit](false) // The ack channel *must* be async
          outCh.send((o.v, ack.out))
          Some(ack.in)
        } else {
          outCh.send(o.v)
          None
        }
        val inCh = outCh.dualIn
        ps match {
          case _: ProcessSystemRunnerImproved =>
            ps.scheduleInCh(inCh)
          case _: ProcessSystemStateMachineMultiStep =>
            ps.smartEnqueue(inCh)
        }
        ack match {
          case Some(inc) => {
            // We must now reschedule ourselves, while we wait for an ack
            // FIXME: allow to specify timeouts
            import effpi.process.dsl.{receive, nil}
            import concurrent.duration.Duration.Inf
            fastEval((env, lp, receive(inc){ _ => nil }(Inf)), stepsLeft)
          }
          case None => {
            lp match {
              case Nil => ()
              case lh :: lt => fastEval((env, lt, lh()), stepsLeft - 1)
            }
          }
        }
      }
      case f: Fork[_] => {
        // TODO: this always gives the same order? this may or may not be a problem
        ps.scheduleProc((env, Nil, f.p()))
        lp match {
          case Nil => ()
          case lh :: lt =>
            fastEval((env, lt, lh()), stepsLeft - 1)
        }
      }
      case n: PNil => lp match {
        case Nil => ()
        case lh :: lt =>
          fastEval((env, lt, lh()), stepsLeft - 1)
      }
      case y: Yield[_] => {
        y.ctx match {
          case Some(c) => c.chan.asInstanceOf[OutChannel[Any]].send(y.v)
          case None => ()
        }
        lp match {
          case Nil => ()
          case lh :: lt =>
            fastEval((env, lt, lh()), stepsLeft - 1)
        }
      }
      case d: Def[_,_,_,_] =>
        fastEval((env + (d.name -> d.pdef), lp, d.in()), stepsLeft - 1)
      case c: Call[_,_] => {
        env.get(c.procvar) match {
          case Some(p) =>
            fastEval(
              (env, lp, p.asInstanceOf[Any => Process](c.arg)), stepsLeft - 1)
          case None =>
            throw new RuntimeException(s"Unbound process variable: ${c.procvar}")
        }
      }
      case s: >>:[_,_] =>
        fastEval((env, s.p2 :: lp, s.p1()), stepsLeft - 1)
      }
    }
  }

}
