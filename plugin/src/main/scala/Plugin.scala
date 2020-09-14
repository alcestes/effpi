// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.verifier

import scala.language.implicitConversions

import dotty.tools.dotc.core.{Names, Types, TypeApplications}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Symbols.requiredClassRef

import dotty.tools.dotc.{plugins, report}
import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd.Tree

import util.optList

/** Verification options
  *
  * @param position Location of the verification annotation.
  * @param keemTmp Preserve temporary files?
  * @param specName Name of the specification, used as infix in the benchmark
  *                 result filename.
  * @param benchmarkReps Benchmark repetitions. (`0` means "no benchmark")
  * @param isBigLTS When behnchmarking: don't count states.
  * @param solver Solving tool and strategy for mCRL2
  */
case class Options(
  position: dotty.tools.dotc.util.SourcePosition,
  keepTmp: Boolean,
  specName: String,
  benchmarkReps: Int,
  isBigLTS: Boolean,
  solver: Solver
)

// Simplified AST for Scala types
sealed abstract trait SimpleType(val orig: Types.Type)
case class Type(override val orig: Types.Type) extends SimpleType(orig)
case class TypeRef(override val orig: Types.TypeRef) extends SimpleType(orig)
case class App(tycon: SimpleType, args: scala.Seq[SimpleType])(orig: Types.Type) extends SimpleType(orig)
case class ProcVar(tpe: SimpleType)(orig: Types.Type) extends SimpleType(orig)
case class MethodType(
  paramNames: List[Names.TermName],
  paramTypes: List[SimpleType],
  ret: SimpleType)(orig: Types.Type) extends SimpleType(orig)
case class ParamRef(override val orig: Types.TermParamRef) extends SimpleType(orig)
case class TermRef(prefix: SimpleType, name: Names.TermName)(orig: Types.Type) extends SimpleType(orig)
case class ExprType(res: SimpleType)(orig: Types.Type) extends SimpleType(orig)
case class OrType(opt1: SimpleType, opt2: SimpleType)(orig: Types.Type) extends SimpleType(orig)

// Representation of dependent behavioural types
sealed abstract class BehType {
  /** Get all mailbox types in the given behaviour */
  def mailboxes(implicit ctx: Context): Set[Mailbox] = this match {
    case PNil | Out(_, _) | RecVar(_) => Set.empty
    case In(chan, cont) => chan match {
      case m: Mailbox => Set(m) ++ cont.ret.mailboxes
      case _ => cont.ret.mailboxes
    }
    case Fork(forked) => forked.mailboxes
    case Or(opt1, opt2) => opt1.mailboxes ++ opt2.mailboxes
    case Seq(pre, post) => pre.mailboxes ++ post.mailboxes
    case RecDef(_name, body) => body.mailboxes
  }

  /** Expand a verification environment to trigger all inputs in a type */
  def fullEnv(observed: Verifier.ObsEnv, env: Verifier.VerifEnv)(implicit ctx: Context): Verifier.VerifEnv = this match {
    case PNil | Out(_,_) | RecVar(_) => env
    case In(chan, cont) => chan match {
      // If the input is observed, expand the environment with a probe for the input
      case v: TypeVar => {
        if (observed.contains(v)) {
          cont.ret.fullEnv(observed, env + SyntTypeVar.fresh(cont.argtype.orig))
        } else {
          cont.ret.fullEnv(observed, env)
        }
      }
      case m: Mailbox => {
        if (observed.contains(m)) {
          cont.ret.fullEnv(observed, env + SyntTypeVar.fresh(cont.argtype.orig))
        } else {
          cont.ret.fullEnv(observed, env)
        }
      }
      case _ => cont.ret.fullEnv(observed, env)
    }
    case Fork(forked) => forked.fullEnv(observed, env)
    case Or(opt1, opt2) => opt2.fullEnv(observed, opt1.fullEnv(observed, env))
    case Seq(pre, post) => post.fullEnv(observed, pre.fullEnv(observed, env))
    case RecDef(_name, body) => body.fullEnv(observed, env)
  }

  /** Substitute the given TypeVar with the given ValueType */
  def subst(x: TypeVar, v: ValueType): BehType = this match {
    case PNil => PNil
    case In(chan, cont) => In(chan.subst(x, v), cont.subst(x, v))
    case Out(chan, value) => Out(chan.subst(x, v), value.subst(x, v))
    case Fork(forked) => Fork(forked.subst(x, v))
    case Or(opt1, opt2) => Or(opt1.subst(x, v), opt2.subst (x, v))
    case Seq(pre, post) => Seq(pre.subst(x, v), post.subst(x, v))
    case RecDef(name, body) => RecDef(name, body.subst(x, v))
    case rv: RecVar => rv
  }
}

case object PNil extends BehType
case class In(chan: ValueType, cont: DepFun) extends BehType
case class Out(chan: ValueType, value: ValueType) extends BehType
case class Fork(forked: BehType) extends BehType
case class Or(opt1: BehType, opt2: BehType) extends BehType
case class Seq(pre: BehType, post: BehType) extends BehType
case class RecDef(name: String, body: BehType) extends BehType
case class RecVar(name: String) extends BehType

sealed abstract trait ValueType(val orig: Types.Type) {
  /** Is it a channel type? (Necessary condition: it is an AppType) */
  def isChannelType: Boolean = {
    isInChannelType || isOutChannelType
  }

  /** Is it an output channel type? (Necessary condition: it is an AppType) */
  def isInChannelType: Boolean = false

  /** Is it an output channel type? (Necessary condition: it is an AppType) */
  def isOutChannelType: Boolean = false

  /** Substitute the given TypeVar with the given ValueType
  *
  * Note that the resulting ValueType will preserve the orig field
  */
  def subst(x: TypeVar, v: ValueType): ValueType = this match {
    case df: DepFun => df.subst(x, v)
    case gt: GroundType => gt
    case at: AppType => at
    case tv: TypeVar => if (tv.name == x.name) v else tv
    case tp: TypeVarPrefix => tp.copy(prefix = tp.prefix.subst(x, v))(tp.orig)
    case mb: Mailbox => mb
    case tb: TypeBounds => tb
    case np: NoPrefix => np
    case sv: SyntTypeVar => sv
  }
}
case class DepFun(arg: TypeVar, argtype: ValueType, ret: BehType)(override val orig: Types.Type) extends ValueType(orig) {
  /** Override method to refine return type */
  override def subst(x: TypeVar, v: ValueType): DepFun = {
    this.copy(ret = ret.subst(x, v))(orig)
  }
}
case class GroundType(override val orig: Types.Type) extends ValueType(orig)
case class AppType(tcons: Types.TypeRef, arg: scala.Seq[SimpleType])(override val orig: Types.Type)(implicit ctx: Context) extends ValueType(orig) {
  override def isInChannelType: Boolean = {
    tcons.typeSymbol == Verifier.DSL.InChannel
  }

  override def isOutChannelType: Boolean = {
    tcons.typeSymbol == Verifier.DSL.OutChannel
  }
}
case class TypeVar(name: Names.Name, widened: ValueType)(orig: Types.Type) extends ValueType(orig)
case class TypeVarPrefix(prefix: ValueType, tvar: TypeVar)(orig: Types.Type) extends ValueType(orig)
case class Mailbox(payload: ValueType)(orig: Types.Type) extends ValueType(orig)

case class TypeBounds(override val orig: Types.TypeBounds) extends SimpleType(orig) with ValueType(orig)
case class NoPrefix()(override val orig: Types.NoPrefix.type = Types.NoPrefix) extends SimpleType(orig) with ValueType(orig)

/** A made-up TypeVar for probing behaviours */
case class SyntTypeVar(name: String)(orig: Types.Type) extends ValueType(orig)
object SyntTypeVar {
  var lastId: Int = 0

  def fresh(orig: Types.Type): SyntTypeVar = synchronized {
    val freshName = s"_synt_${lastId}"
    lastId += 1
    SyntTypeVar(freshName)(orig)
  }
}

object Verifier {
  type ObsEnv = Set[TypeVar | Mailbox] // Observables
  type SomeTypeVar = TypeVar | SyntTypeVar
  type VerifEnv = Set[SomeTypeVar | Mailbox]

    // Classes from the process and actor DSLs
  protected[verifier] object DSL {
    def InChannel(implicit ctx: Context) = {
      requiredClassRef("effpi.channel.InChannel").symbol.asClass
    }

    def OutChannel(implicit ctx: Context) = {
      requiredClassRef("effpi.channel.InChannel").symbol.asClass
    }

    def Behavior(implicit ctx: Context) = {
      requiredClassRef("effpi.actor.dsl.Behavior").symbol.asClass
    }
    // def Actor(implicit ctx: Context) = {
    //   requiredClassRef("effpi.actor.dsl.Actor").symbol.asClass
    // }
    def ActorCtx(implicit ctx: Context) = {
      requiredClassRef("effpi.actor.ActorCtx").symbol.asClass
    }

    def Mailbox(implicit ctx: Context) = {
      requiredClassRef("effpi.actor.Mailbox").symbol.asClass
    }

    def Process(implicit ctx: Context) = {
      requiredClassRef("effpi.process.Process").symbol.asClass
    }
    def Nil(implicit ctx: Context) = {
      requiredClassRef("effpi.process.PNil").symbol.asClass
    }
    def Def(implicit ctx: Context) = {
      requiredClassRef("effpi.process.Def").symbol.asClass
    }
    def Call(implicit ctx: Context) = {
      requiredClassRef("effpi.process.Call").symbol.asClass
    }
    def In(implicit ctx: Context) = {
      requiredClassRef("effpi.process.In").symbol.asClass
    }
    def Out(implicit ctx: Context) = {
      requiredClassRef("effpi.process.Out").symbol.asClass
    }
    def Seq(implicit ctx: Context) = {
      requiredClassRef("effpi.process.>>:").symbol.asClass
    }
    def Fork(implicit ctx: Context) = {
      requiredClassRef("effpi.process.Fork").symbol.asClass
    }
    def ProcVar(implicit ctx: Context) = {
      requiredClassRef("effpi.process.ProcVar").symbol.asClass
    }
  }

  /** Return true if the given types might communicate.
  *
  * TODO: move somewhere else.
  *
  * @param retrying guard whether the method is calling itself recursively
  */
  def canCommunicate(t1: ValueType, t2: ValueType,
                     retrying: Boolean = false)
                    (implicit ctx: Context): Boolean = {
    val ret = (t1, t2) match {
      case (TypeVar(name1, w1), TypeVar(name2, w2)) if name1 == name2 => {
        assert(w1 == w2)
        true
      }
      case (TypeVarPrefix(pfx1, tv1), TypeVarPrefix(pfx2, tv2)) => {
        (pfx1 == pfx2) && canCommunicate(tv1, tv2, retrying)
      }
      case t12 @ (AppType(r1, scala.Seq(p1)), AppType(r2, scala.Seq(p2))) => {
        (t12._1.isInChannelType && t12._2.isOutChannelType &&
          (p2.orig <:< p1.orig))
      }
      case t12 @ (AppType(r, scala.Seq(payload)), TypeVar(_n, widened)) => {
        t12._1.isInChannelType && widened.isOutChannelType && (widened match {
          case AppType(r, scala.Seq(arg)) => arg.orig <:< payload.orig
          case _ => {
            throw new IllegalStateException(s"BUG: expected OutChannel, got ${widened}")
          }
        }) ||
        t12._1.isOutChannelType && widened.isInChannelType && (widened match {
          case AppType(r, scala.Seq(arg)) => payload.orig <:< arg.orig
          case _ => {
            throw new IllegalStateException(s"BUG: expected InChannel, got ${widened}")
          }
        })
      }
      case t12 @ (Mailbox(payload), AppType(r, scala.Seq(arg))) => {
        t12._2.isOutChannelType && (arg.orig <:< payload.orig)
      }
      case t12 @ (Mailbox(payload), TypeVar(_n, widened)) => {
        widened.isOutChannelType && (widened match {
          case AppType(r, scala.Seq(arg)) => arg.orig <:< payload.orig
          case _ => {
            throw new IllegalStateException(s"BUG: exected OutChannel, got ${widened}")
          }
        })
      }
      case _ if !retrying => canCommunicate(t2, t1, true)
      case _ => false
    }
    //if (ret) report.log(s"Can communicate:\n${t1.orig.show}\n${t2.orig.show}")
    //else report.log(s"Cannot communicate:\n${t1.orig.show}\n${t2.orig.show}")
    ret
  }
}

private val pluginName = "effpiVerifier" // The name of this plugin

class EffpiVerifier extends plugins.StandardPlugin {
  val name: String = pluginName
  override val description: String = "verify behavioural properties of effpi programs"

  // Plugin options
  private var optKeepTmp = false // Keep temporary files?
  private var optSkipLts = false // Skip LTS generation?
  private var optBenchOverride: Option[Int] = None // Override benchmark config?
  
  def init(options: List[String]): List[plugins.PluginPhase] = {
    options.foreach { o =>
      if (o == "keep-tmp") {
        optKeepTmp = true
      } else if (o == "skip-lts") {
        optSkipLts = true
      } else if (o.startsWith("timestamp=")) {
        // Ignore this option
      } else if (o.startsWith("bench-override=")) {
        optBenchOverride = Some(o.split("=")(1).toInt)
      } else {
        // FIXME: throw a more meaningful exception here?
        throw new RuntimeException(s"Invalid plugin option: ${o}")
      }
    }
    (new VerifierPhase(optKeepTmp, optSkipLts, optBenchOverride)) :: Nil
  }
}

class VerifierPhase(keepTmp: Boolean,
                    skipLts: Boolean,
                    benchOverride: Option[Int]) extends plugins.PluginPhase {
  import Verifier.DSL

  val phaseName = pluginName

  override val runsAfter = Set(dotty.tools.dotc.typer.FrontEnd.name)
  // override val runsBefore = Set(transform.FirstTransform.name)

  // Names of annotation arguments.
  // NOTE: must be kept in sync with the effpi.verifier.verify annotation
  private val ARG_PROPERTY = "property"
  private val ARG_BENCHMARK = "benchmark"
  private val ARG_SPEC_NAME = "spec_name"
  private val ARG_BIGLTS = "big_lts"
  private val ARG_SOLVER = "solver"
  private val ARGS_SET = Set(
    ARG_PROPERTY, ARG_SPEC_NAME, ARG_BENCHMARK, ARG_BIGLTS, ARG_SOLVER)

  def Annotation(implicit ctx: Context) = {
    requiredClassRef("effpi.verifier.verify").symbol.asClass
  }

  // Useful Scala classes
  private object Scala {
    def Function1(implicit ctx: Context) = {
      requiredClassRef("scala.Function1").symbol.asClass
    }
    def ImplicitFunction1(implicit ctx: Context) = {
      requiredClassRef("scala.ImplicitFunction1").symbol.asClass
    }
    def ContextFunction1(implicit ctx: Context) = {
      requiredClassRef("scala.ContextFunction1").symbol.asClass
    }
    def Unit(implicit ctx: Context) = {
      requiredClassRef("scala.Unit").symbol.asClass
    }
  }

  private def toString(tpe: Types.Type)(implicit ctx: Context): String = {
    ctx.printer.toText(tpe).mkString(80, false)
    //tpe.show
  }

  override def transformDefDef(tree: tpd.DefDef)
                              (implicit ctx: Context): Tree = {
    if (!tree.symbol.hasAnnotation(Annotation)) {
      return tree
    }
    // Type widening enlarges the type of x from, e.g., Int(x) to Int
    val obs: Verifier.ObsEnv = tree.vparamss.flatten.map { x =>
      val vte = for {
        w <- simplify(x.tpe.widen)
        vt <- toValueType(w)
      } yield vt
      if (vte.isEmpty) {
        report.warning(s"Cannot check: ${tree.show}")
      }
      TypeVar(x.name, vte.get)(x.tpe)
    }.toSet
    check(tree, tree.tpt.tpe, obs)
  }

  override def transformValDef(tree: tpd.ValDef)
                              (implicit ctx: Context): Tree = {
    check(tree, tree.tpt.tpe, Set())
  }

  override def transformTypeDef(tree: tpd.TypeDef)
                               (implicit ctx: Context): Tree = {
    
    check(tree, tree.rhs.tpe, Set())
  }

  /** Check the given type, which is assumed to belong to the given tree.
  *
  * @param observable the variables to be observed
  */
  private def check(tree: Tree, tpe: Types.Type,
                    observables: Verifier.ObsEnv)
                   (implicit ctx: Context): Tree = {
    if (tree.symbol.hasAnnotation(Annotation)) {
      val annotOpt = getAnnotation(tree)
      if (annotOpt.isEmpty) {
        report.error("Unable to parse annotations", tree.sourcePos)
        return tree
      }
      val annot = annotOpt.get
      val unknownArgs = annot.keySet.filter { !ARGS_SET.contains(_) }
      if (!unknownArgs.isEmpty) {
        report.error(s"Invalid annotation arguments: ${unknownArgs}", tree.sourcePos)
        return tree
      }
      if (!annot.keySet.contains(ARG_PROPERTY)) {
        report.error("No property has been specified", tree.sourcePos)
        return tree
      }
      val propStr = annot(ARG_PROPERTY)

      val benchmarkRepsStr = annot.getOrElse(ARG_BENCHMARK, "0")
      val benchmarkReps = try {
        val v = benchmarkRepsStr.toInt
        if (v < 0) {
          throw new NumberFormatException()
        }
        v
      } catch {
        case e: NumberFormatException => {
          report.error(s"Argument ${ARG_BENCHMARK} must be a non-negative integer (got '${benchmarkRepsStr}'")
          return tree
        }
      }

      val isBigLTS: Boolean = annot.getOrElse(ARG_BIGLTS, "false") match {
        case "true"  => true
        case "false" => false
        case other => {
          report.error(s"Argument ${ARG_BIGLTS} must be 'true' or 'false' (got '${other}'")
          return tree
        }
      }

      val solver: Solver = annot.getOrElse(ARG_SOLVER, "pbessolve") match {
        case "pbessolve" => pbessolve
        case "pbes2bool_breadth" => pbes2bool_breadth
        case "pbes2bool_depth" => pbes2bool_depth
        case other => {
          throw new RuntimeException(s"BUG: unsupported value for ${ARG_SOLVER} (got '${other}'")
        }
      }

      val specName: String = annot.getOrElse(ARG_SPEC_NAME, "spec")

      // When benchmarking, we do not remove the temporary dir
      val options = Options(tree.sourcePos, keepTmp || (benchmarkReps > 0),
                            specName,
                            benchOverride.getOrElse(benchmarkReps),
                            isBigLTS || skipLts,
                            solver)

      val prop = mcrl2.Property.apply(propStr, observables, options)

      if (prop.isLeft) {
        // FIXME: remove null, refactor for the Scala 2.13 stdlib
        report.error(s"Property error: ${prop.swap.getOrElse(null)}", tree.sourcePos)
        return tree
      }

      // Trim the observables, only considering those mentioned by the property
      // FIXME: remove null, refactor for the Scala 2.13 stdlib
      val pvars = prop.getOrElse(null).variables
      val obs = observables.filter { o =>
        pvars contains { o match {
          case TypeVar(name, _) => name.toString
          case Mailbox (_)=> mcrl2.Spec.MAILBOX_ALIAS // NOTE: assume 1 mailbox
        } }
      }
      report.log(s"Analysing type:\n${toString(tpe)}")
      report.log(s"Observables:\n${observables}")
      report.log(s"Property variables:\n${pvars}")
      report.log(s"Observed:\n${obs}")

      simplify(tpe) match {
        case None => {
          report.warning(s"Unable to simplify: ${toString(tpe)}", tree.sourcePos)
        }
        case Some(s) => {
          report.log(s"Simplified type:\n${s}")  
          toBehType(s) match {
            case None => report.warning(s"Unable to determine behavioural type from: ${toString(tpe)}", tree.sourcePos)
            case Some(b) => {
              report.log(s"Behavioral type:\n${b}")
              val fullObs = obs ++ b.mailboxes // We also observe all mailboxes
              val baseEnv: Verifier.VerifEnv = Set(fullObs.toSeq:_*)
              val fullEnv = b.fullEnv(fullObs, baseEnv)
              assert(fullObs.forall { o => fullEnv.contains(o) } )
              ccst.CCST(b, fullObs, fullEnv, Set()) match {
                case None => {
                  report.warning(s"Unable to determine CCST spec from: ${toString(tpe)}", tree.sourcePos)
                }
                case Some(rawccs0) => {
                  report.log(s"CCST spec:\n${rawccs0}")
                  // TODO: here we should allow for a fixpoint loop, for
                  // greater analysis accuracy
                  // NOTE: ccst.CCST() succeeded: we assume it succeeds again
                  val rawccs = ccst.CCST(b, fullObs, fullEnv, rawccs0.outputs).get
                  val ccs = rawccs.barendregt
                  report.log(s"CCST spec with Barendregt convention:\n${ccs}")
                  val mspec = mcrl2.Spec(ccs)
                  report.log(s"mCRL2 spec:\n${mspec.show}")
                  
                  // FIXME: remove null, refactor for the Scala 2.13 stdlib
                  val verifier = mcrl2.Verifier(mspec, prop.getOrElse(null),
                                                options)
                  
                  // Skip verification if requested via compiler options
                  benchOverride match {
                    case Some(-1) => {
                      verifier.close()
                      return tree
                    }
                    case _ => ()
                  }

                  verifier.verify(fullEnv) match {
                    case Left(err) => {
                      report.error(s"Verification failed: ${err}")
                    }
                    case Right(res) => {
                      if (!res) {
                        report.warning(s"The property does not hold", tree.sourcePos)
                      }
                    }
                  }
                  verifier.close()
                }
              }
            }
          }
        }
      }
    }
    tree
  }

  private def simplify(t: Types.Type)
                      (implicit ctx: Context): Option[SimpleType] = (t match {
    case t1: Types.TypeRef =>
      t1.safeDealias match {
        case tr: Types.TypeRef => Some(TypeRef(tr))
        case other => simplify(other)
      }
    case app @ Types.AppliedType(tycon, args) => {
      // report.log(s"Simplifying applied type:\n${app.show}")
      val dealiased = tycon.safeDealias
      // report.log(s"Dealiased tycon:\n${dealiased.show}")
      dealiased match {
        case t: Types.TypeLambda => {
          val reducer = new TypeApplications.Reducer(t, args)
          val reduced = reducer(t.resType)
          if (reducer.allReplaced) {
            // report.log(s"Applied type reduced to:\n${reduced.show}")
            simplify(reduced)
          } else {
            report.warning(s"Cannot fully reduce TypeLambda:\n${t.show}\n${reduced.show}")
            None
          }
        }
        case _ => {
          for {
            st <- simplify(tycon)
            sa <- optList(args.map(simplify(_)))
          } yield App(st, sa)(app)
        }
      }
    }
    case ot @ Types.OrType(opt1, opt2) => {
      for { o1 <- simplify(opt1); o2 <- simplify(opt2) } yield OrType(o1, o2)(ot)
    }
    case tl: Types.TypeLambda => {
      // HKTypeLambdas should only be yielded by process variables (or their
      // subtypes) from the process DSL; we strip the argument
      if (tl <:< DSL.ProcVar.typeRef) {
        tl.resType match {
          case Types.AppliedType(tycon, args) => {
            assert(args.length == 1)
            for { tc <- simplify(tycon) } yield ProcVar(tc)(tl)
          }
          case _ => None
        }
      } else {
        report.warning(s"Found unsupported TypeLambda: ${tl.show}")
        None
      }
    }
    case Types.RefinedType(p, n, i) => {
      // Here we are only interested in the refined type information `i`:
      // the refinement parent `p` is less precise, and loses type variables
      simplify(i)
    }
    case mt: Types.MethodType => {
      // FIXME: can we refine paramInfos?
      mt.paramInfos.foreach { pi => pi.paramNamess ; report.log(s"Param info: ${pi.show}")}
      for {
        pl <- optList(mt.paramInfos.map(simplify(_)))
        r <- simplify(mt.resType)
      } yield MethodType(mt.paramNames, pl, r)(mt)
    }
    case tpr: Types.TermParamRef => Some(ParamRef(tpr))
    case tr: Types.TermRef => for {
      pfx <- simplify(tr.prefix)
    } yield TermRef(pfx, tr.implicitName)(tr)
    case et @ Types.ExprType(resType) => for {
      r <- simplify(resType)
    } yield ExprType(r)(et)
    case Types.NoPrefix => Some(NoPrefix()())
    case tb: Types.TypeBounds => Some(TypeBounds(tb))
    case _ => None
  }) orElse {
    report.log(s"Cannot simplify: ${toString(t)} (${t})")
    None
  }

  private def toBehType(t: SimpleType)
                       (implicit ctx: Context): Option[BehType] = (t match {
    case TypeRef(r) => {
      if (r.typeSymbol == DSL.Nil) Some(PNil)
      else None
    }
    case App(TypeRef(r), args) => {
      if (r.typeSymbol == Scala.ImplicitFunction1 ||
          r.typeSymbol == Scala.ContextFunction1) {
        // This should be an Actor[_,_] type
        assert(args.length == 2)
        args(0) match {
          case App(TypeRef(r2), args2) => {
            assert(args2.length == 1)
            if (r2.typeSymbol == DSL.ActorCtx) {
              // args2(0) is the actor mailbox type
              // We are only interested in the type body now.
              // We will recognise uses of mailbox(es) as In[Mailbox...]
              toBehType(args(1))
            } else {
              report.log(s"Unsupported implicit function application: ${r2}")
              None
            }
          }
          case _ => None
        }
      } else if (r.typeSymbol == DSL.In) {
        assert(args.length == 3)
        for {
          a0 <- toValueType(args(0))
          // NOTE: we ignore the 2nd argument of DSL.In, that is redundant
          a2 <- toDepFun(args(2))
        } yield In(a0, a2)
      } else if (r.typeSymbol == DSL.Out) {
        assert(args.length == 2)
        for {
          a0 <- toValueType(args(0))
          a1 <- toValueType(args(1))
        } yield Out(a0, a1)
      } else if (r.typeSymbol == DSL.Seq) {
        assert(args.length == 2)
        for {
          a0 <- toBehType(args(0))
          a1 <- toBehType(args(1))
        } yield Seq(a0, a1)
      } else if (r.typeSymbol == DSL.Fork) {
        assert(args.length == 1)
        for { a0 <- toBehType(args(0)) } yield Fork(a0)
      } else if (r.typeSymbol == DSL.Def) {
        assert(args.length == 4)
        args(0) match {
          case ProcVar(TypeRef(ref)) => args(1) match {
            // For now, we only support simple recursion without parameters...
            case TypeRef(ur) if (ur <:< Scala.Unit.typeRef) => {
              // ...and correspondingly, we expect args(2) == args(3)
              if (args(2) == args(3)) {
                for { body <- toBehType(args(3)) } yield RecDef(ref.show, body)
              } else {
                report.log(s"Def: mismatching body and continuation:\n${args(3)}\n${args(4)}")
                None
              }
            }
            case _ => {
              // For now, we only support simple recursion without parameters
              report.log(s"Def: unsupported recursion argument type: ${args(1)}")
              None
            }
          }
          case _ => {
            report.log(s"Def: unsupported recursion variable: ${args(0)}")
            None
          }
        }
      } else if (r.typeSymbol == DSL.Call) {
        assert(args.length == 2)
        args(0) match {
          case ProcVar(TypeRef(name)) => args(1) match {
            case TypeRef(ur) if (ur <:< Scala.Unit.typeRef) => {
              Some(RecVar(name.show))
            }
            case _ => {
              // For now, we only support simple recursion without parameters
              report.log(s"Call: unsupported recursion argument type: ${args(1)}")
              None
            }
          }
          case _ => {
            report.log(s"Call: unsupported recursion variable: ${args(0)}")
            None
          }
        }
      } else {
        report.log(s"App: unsupported TypeSymbol: ${r.typeSymbol}")
        None
      }
    }
    case OrType(opt1, opt2) => {
      for { o1 <- toBehType(opt1); o2 <- toBehType(opt2) } yield Or(o1, o2)
    }
    case _ => None
  }) orElse {
    report.log(s"Cannot convert to behavioral type:\n${t}")
    None
  }

  private def toValueType(t: SimpleType)
                         (implicit ctx: Context): Option[ValueType] = t match {
    case a @ App(TypeRef(r), args) if (r <:< DSL.Mailbox.typeRef) => {
      assert(args.length == 1)
      for {
        argt <- toValueType(args(0))
      } yield Mailbox(argt)(a.orig)
    }
    case a @ App(TypeRef(r), args) => Some(AppType(r, args)(a.orig))
    case TypeRef(r) if (!(r <:< DSL.Process.typeRef)) => Some(GroundType(r))
    case tr @ TermRef(prefix, name) => for {
      pfx <- toValueType(prefix)
      widened <- simplify(tr.orig.widen)
      vt <- toValueType(widened)
    } yield pfx match {
      case _: NoPrefix => TypeVar(name, vt)(tr.orig)
      case _ => TypeVarPrefix(pfx, TypeVar(name, vt)(tr.orig))(tr.orig)
    }
    case ParamRef(r) => toTypeVar(t)
    case np: NoPrefix => Some(np)
    case tb: TypeBounds => Some(tb)
    case _ => {
      report.log(s"Cannot convert to ValueType: ${t}")
      None
    }
  }

  private def toTypeVar(t: SimpleType)
                       (implicit ctx: Context): Option[TypeVar] = t match {
    case ParamRef(r) => for {
      w <- simplify(r.widen)
      vt <- toValueType(w)
    } yield TypeVar(r.paramName.toTermName, vt)(r)
    case _ => None
  }

  private def toDepFun(t: SimpleType)
                      (implicit ctx: Context): Option[DepFun] = (t match {
    case m @ MethodType(paramNames, paramTypes, ret) => {
      assert(paramNames.length == paramTypes.length)
      if (paramNames.length != 1) {
        report.log(s"toDepFun: cannot convert MethodType with 2+ parameters: ${t}")
        None
      } else {
        for {
          r <- toBehType(ret)
          argt <- toValueType(paramTypes(0))
        } yield DepFun(TypeVar(paramNames(0), argt)(argt.orig), argt, r)(m.orig)
      }
    }
    // case App(TypeRef(r), arg :: ret :: Nil, _orig) if (r <:< Scala.Function1.typeRef) => for {
    //   argt <- toValueType(arg)
    //   rett <- toBehType(ret)
    // } yield DepFun(/* TODO: create a fresh name here*/, argt, rett)
    case _ => None
  }) orElse {
    report.log(s"Cannot convert to ${DepFun.getClass.getName}: ${t}")
    None
  }

  // Return 
  // This function assumes that the annotation is present --- e.g., because
  // it was found through symbol.hasAnnotation
  private def getAnnotation(tree: Tree)
                           (implicit ctx: Context): Option[Map[String, String]] =
    tree.symbol.annotations.headOption.flatMap { ann =>
      ann.arguments.foldLeft(Some(Map()): Option[Map[String, String]]) { (acc, arg) =>
      (for (args <- acc) yield {
        arg match {
          case Trees.Select(Trees.Ident(n), _) if (n.toString == "verify") => {
            // This is just the name of the annotation, we can skip
            Some(args)
          }
          case Trees.Ident(_) => {
            // This is an omitted default argument; we already handle them
            Some(args)
          }
          case Trees.NamedArg(name, arg2) => arg2 match {
            case Trees.Literal(c) =>
              Some(args + (name.toString -> c.stringValue))
            case Trees.Ident(c) =>
              Some(args + (name.toString -> c.toString))
            case other => {
              report.error(s"Unexpected annotation: ${other}")
              None
            }
          }
          case other => {
            report.error(s"Unexpected annotation: ${other}")
            None
          }
        }
      }).getOrElse(None)
    }
  }
}
