// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.system

import java.lang.Runnable

import scala.util.{Failure, Success, Try}
import scala.concurrent.duration.Duration

import effpi.channel.{InChannel, OutChannel, ChannelStatus}

protected[system] class InputExecutor(ps: ProcessSystem, stepsLeft: Int = 10) extends Runnable {

  import effpi.process._

  override def run() = {
    while (ps.alive) {
      val maybeInCh = ps.consumeInCh()
      maybeInCh match {
        case Some(in) =>
          ps match {
            case _: ProcessSystemRunnerImproved =>
              ()
            case _: ProcessSystemStateMachineMultiStep =>
              in.schedulingStatus.set(ChannelStatus.running)
          }
          in.dequeue() match {
            case Some(proc) =>
              multiInEval(proc, stepsLeft)
            case None =>
              ps match {
                case _: ProcessSystemRunnerImproved =>
                  ps.scheduleInCh(in)
                case _: ProcessSystemStateMachineMultiStep =>
                  ps.smartUnschedule(in)
              }
          }
        case None =>
          ()
      }
    }
  }

  @annotation.tailrec
  private def multiInEval(
    proc: (Map[ProcVar[_], (_) => Process], List[() => Process], Process),
    stepsLeft: Int
  ): Unit = {
    if (stepsLeft == 0) {
      ps.scheduleProc(proc)
    } else {
      val (env, lp, p) = proc
      p match {
        case i: In[_,_,_] =>
          i.channel.poll() match {
            case Some(v) =>
              val cont = i.cont.asInstanceOf[Any => Process](v)
              ps match {
                case _: ProcessSystemRunnerImproved =>
                  ()
                case _: ProcessSystemStateMachineMultiStep =>
                  ps.forceSchedule(i.channel)
              }
              multiInEval((env, lp, cont), stepsLeft - 1)
            case None =>
              i.channel.enqueue((env, lp, i.asInstanceOf[In[InChannel[Any], Any, Any => Process]]))

              ps match {
                case _: ProcessSystemRunnerImproved =>
                  ()
                case _: ProcessSystemStateMachineMultiStep =>
                  ps.smartUnschedule(i.channel)
              }
          }
        case o: Out[_,_] =>
          val outCh = o.channel.asInstanceOf[OutChannel[Any]]
          outCh.send(o.v)
          val inCh = outCh.dualIn
          ps match {
            case _: ProcessSystemRunnerImproved =>
              ps.scheduleInCh(inCh)
            case _: ProcessSystemStateMachineMultiStep =>
              ps.smartEnqueue(inCh)
          }
          lp match {
            case Nil =>
              ()
            case lh :: lt =>
              multiInEval((env, lt, lh()), stepsLeft - 1)
          }
        case f: Fork[_] =>
          // TODO: this always gives the same order? this may or may not be a problem
          ps.scheduleProc((env, Nil, f.p()))
          lp match {
            case Nil => ()
            case lh :: lt =>
              multiInEval((env, lt, lh()), stepsLeft - 1)
          }
        case n: PNil => lp match {
          case Nil => ()
          case lh :: lt =>
            multiInEval((env, lt, lh()), stepsLeft - 1)
        }
        case y: Yield[_] =>
          y.ctx match {
            case Some(c) => c.chan.asInstanceOf[OutChannel[Any]].send(y.v)
            case None => ()
          }
          lp match {
            case Nil => ()
            case lh :: lt =>
              multiInEval((env, lt, lh()), stepsLeft - 1)
          }
        case d: Def[_,_,_,_] =>
          multiInEval((env + (d.name -> d.pdef), lp, d.in()), stepsLeft - 1)
        case c: Call[_,_] =>
          env.get(c.procvar) match {
            case Some(p) =>
              multiInEval(
                (env, lp, p.asInstanceOf[Any => Process](c.arg)), stepsLeft - 1)
            case None =>
              throw new RuntimeException(s"Unbound process variable: ${c.procvar}")
          }
        case s: >>:[_,_] =>
          multiInEval((env, s.p2 :: lp, s.p1()), stepsLeft - 1)
      }
    }
  }

}
