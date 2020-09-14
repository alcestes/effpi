// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
/** CCS-like calculus for encoding typing behavioural types */
package effpi.verifier.ccst

import effpi.{verifier => verif}
import effpi.verifier.{ValueType, TypeVar}

import effpi.verifier.util.optList

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report

/** A pair combining a channel and a payload type */
protected[verifier] case class ChanPayload(chan: ValueType, payload: ValueType)

protected[verifier] sealed abstract class CCST {
  /** Is this CCST term sequential? */
  def sequential: Boolean = this match {
    case End => true
    case Out(_chan, _payload, cont) => cont.sequential
    case Branch(choices) => choices.values.forall {cont => cont.sequential}
    case Or(p1, p2) => p1.sequential && p2.sequential
    case Par(_, _) => false
    case Rec(_recvar, body) => body.sequential
    case RecVar(_) => true
  }

  /** Is this CCST term recursive? */
  def recursive: Boolean = this match {
    case End => false
    case Out(_chan, _payload, cont) => cont.recursive
    case Branch(choices) => choices.values.exists {cont => cont.recursive}
    case Or(p1, p2) => p1.recursive || p2.recursive
    case Par(p1, p2) => p1.recursive || p2.recursive
    case Rec(_recvar, _body) => true
    case RecVar(_) => false // Unbound recursion variable: report?
  }

  /** Return a CCST term where each End is replaced by @cont.
  *
  * @throws IllegalArgumentException if @this is recursive, or not sequential
  */
  def concat(cont: CCST): CCST = this match {
    case End => cont
    case Out(chan, payload, cnt) => Out(chan, payload, cnt.concat(cont))
    case Branch(choices) => Branch(choices.map { kv => (kv._1, kv._2.concat(cont)) })
    case Or(p1, p2) => Or(p1.concat(cont), p2.concat(cont))
    case _ => {
      throw new IllegalArgumentException(s"Cannot concatenate ${this} and ${cont}")
    }
  }

  /** Return all pairs of input+payload occurring in the term. */
  lazy val inputs: Set[ChanPayload] = prefixes(true)

  /** Return all pairs of output+payload occurring in the term */
  lazy val outputs: Set[ChanPayload] = prefixes(false)
  
  /** Return all approximate uses of the given type variable `v`
   *  (i.e., all types occurring as either input or output subjects, that
   *  are supertypes of `v`.
   */
   // TODO: ideally, we should not have `ctx` in this class...
   def approxUses(v: TypeVar)(implicit ctx: Context): Set[ValueType] = {
    val chans = (inputs ++ outputs).map(_.chan)
    report.log(s"Channels used for I/O:\n${chans}")
    chans.filter { c =>
      // Here we compare both:
      //   - `v` with `c` (it works when `v` derives from `v.type` in a spec)
      //   - `v.widened` with `c` (it works when `v` is an observable variable)
      val res = v.orig <:< c.orig || v.widened.orig <:< c.orig
      report.log(s"Comparing for approximation (getting ${res}):\n${v.orig} <:< ${c.orig} OR\n${v.widened.orig} <:< ${c.orig}")
      res
    }
   }

  /** Return input prefixes, or output prefixes if arg is false */
  private def prefixes(inputs: Boolean): Set[ChanPayload] = this match {
    case End => Set.empty
    case Out(chan, payload, cont) => cont.prefixes(inputs) ++ {
      if (!inputs) Set(ChanPayload(chan, payload))
      else Set.empty
    }
    case Branch(choices) => choices.foldLeft(Set.empty) { (acc, c) =>
      acc ++ c._2.prefixes(inputs) ++ {
        if (inputs) Set(ChanPayload(c._1.chan, c._1.payload))
        else Set.empty
      }
    }
    case Or(p1, p2) => p1.prefixes(inputs) ++ p2.prefixes(inputs)
    case Par(p1, p2) => p1.prefixes(inputs) ++ p2.prefixes(inputs)
    case Rec(_recvar, body) => body.prefixes(inputs)
    case RecVar(_) => Set.empty
  }

  /** Substitute the given variable with the given term. */
  def subst(v: RecVar, repl: CCST): CCST = this match {
    case e @ End => e
    case o @ Out(_chan, _payload, cont) => o.copy(cont = cont.subst(v, repl))
    case b @ Branch(choices) => b.copy(choices = choices.map { inCont =>
      (inCont._1, inCont._2.subst(v, repl))
    })
    case Or(p1, p2) => Or(p1.subst(v, repl), p2.subst(v, repl))
    case Par(p1, p2) => Par(p1.subst(v, repl), p2.subst(v, repl))
    case r @ Rec(recvar, body) if (recvar == v) => r // Variable is rebound
    case r @ Rec(_recvar, body) => r.copy(body = body.subst(v, repl))
    case rv @ RecVar(_) if (rv == v) => repl
    case rv @ RecVar(_) => rv
  }

  /** Apply the Barendregt convention,: make bound rec variables unique. */
  lazy val barendregt: CCST = {
    val seen = scala.collection.mutable.Set[RecVar]() // Collects known vars
    // Internal function that traverses the CCST term while updating "seen"
    def barenloop(t: CCST): CCST = t match {
      case e @ End => e
      case o @ Out(_chan, _payload, cont) => o.copy(cont = barenloop(cont))
      case b @ Branch(choices) => b.copy(choices = choices.map { inCont =>
        (inCont._1, barenloop(inCont._2))
      })
      case Or(p1, p2) => Or(barenloop(p1), barenloop(p2))
      case Par(p1, p2) => Par(barenloop(p1), barenloop(p2))
      case r @ Rec(recvar, body) => if (!seen.contains(recvar)) {
        seen += recvar
        r.copy(body = barenloop(body))
      } else {
        // NOTE: the following might repeat, until an unseen recvar is found
        // (and its name might have multiple "B" suffixes)
        val rv2 = RecVar(recvar.name ++ "B") // Mangle the variable name
        barenloop(Rec(rv2, body.subst(recvar, rv2)))
      }
      case rv @ RecVar(_) => rv // Unbound recursion variable: report?
    }

    barenloop(this)
  }
}
case object End extends CCST
case class Out(chan: ValueType, payload: ValueType, cont: CCST) extends CCST
case class In(chan: ValueType, payload: ValueType)
case class Branch(choices: Map[In, CCST]) extends CCST
case class Or(p1: CCST, p2: CCST) extends CCST
case class Par(p1: CCST, p2: CCST) extends CCST
case class Rec(recvar: RecVar, body: CCST) extends CCST
case class RecVar(name: String) extends CCST

object CCST {
  import effpi.verifier.Verifier

  /** Generate a CCST spec, for the given observables and verification probes,
   *  and the given set of potential outputs. */
  def apply(t: verif.BehType, obs: Verifier.ObsEnv, probes: Verifier.VerifEnv,
            outputs: Set[ChanPayload])
           (implicit ctx: Context): Option[CCST] = t match {
    case verif.PNil => Some(End)
    case verif.Out(chan, value) => Some(Out(chan, value, End))
    case verif.In(chan, depfun) => {
      // Is the input channel being observed?
      val observed = obs.exists { o => chan.orig <:< o.orig }
      val branches: Option[List[(In, CCST)]] = optList(
        List(for {
           // Basic branch, for coarse input
           b <- apply(depfun.ret.subst(depfun.arg, depfun.argtype), obs, probes, outputs)
        } yield (In(chan, depfun.argtype), b)) ++ {
          if (observed) {
            // Add a branch for each probe that can be passed to depfun
            probes.filter( p =>
              p.orig <:< depfun.argtype.orig
            ).map( p => {
              report.log(s"Expanding branching for:\nprobe: ${p}\ntype: ${depfun.argtype.orig}")
              for {
                b <- apply(depfun.ret.subst(depfun.arg, p), obs, probes, outputs)
              } yield (In(chan, p), b)
            } ).toList
          } else Nil
        } ++ {
          // Add a branch for each output that might interact
          import effpi.verifier.Verifier.canCommunicate
          outputs.filter( o =>
            canCommunicate(chan, o.chan)
          ).map( o => {
            report.log(s"Expanding branching for:\noutput on: ${o.chan}\ntype: ${o.payload.orig}")
            for {
                b <- apply(depfun.ret.subst(depfun.arg, o.payload), obs, probes, outputs)
              } yield (In(o.chan, o.payload), b)
          } ).toSeq
        }
      )
      branches match {
        case None => {
          report.log(s"CCST(): cannot convert to branching: ${t}")
          None
        }
        case Some(bs) => {
          Some(Branch(Map(bs:_*)))
        }
      }
    }
    case verif.Seq(verif.Fork(b1), b2) => for {
      p1 <- apply(b1, obs, probes, outputs)
      p2 <- apply(b2, obs, probes, outputs)
    } yield Par(p1, p2)
    case verif.Seq(pre, post) => for {
      pre2 <- apply(pre, obs, probes, outputs)
      if (pre2.sequential && !pre2.recursive)
      post2 <- apply(post, obs, probes, outputs)
    } yield pre2.concat(post2)
    case verif.Or(b1, b2) => for {
      p1 <- apply(b1, obs, probes, outputs)
      p2 <- apply(b2, obs, probes, outputs)
    } yield Or(p1, p2)
    case verif.Fork(beh) => apply(beh, obs, probes, outputs)
    case verif.RecDef(name, body) => for {
      body2 <- apply(body, obs, probes, outputs)
    } yield Rec(RecVar(name), body2)
    case verif.RecVar(name) => Some(RecVar(name))
  }
}
