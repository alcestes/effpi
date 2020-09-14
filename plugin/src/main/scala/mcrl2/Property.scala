// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.verifier.mcrl2

import effpi.verifier.{Options, Verifier, TypeVar}
import effpi.verifier.util

import java.nio.file.{Files, Path, Paths}

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions
import scala.util.parsing.combinator._

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report

/** A raw property produced by parsing. */
private sealed abstract class RawProperty
private object RawProperty {
  case class App(name: String, args: List[List[String]]) extends RawProperty
  case class And(p1: RawProperty, p2: RawProperty) extends RawProperty
  case class Or(p1: RawProperty, p2: RawProperty) extends RawProperty
  case class Not(p: RawProperty) extends RawProperty
}

object PropertyParser extends RegexParsers {
  def identifier: Parser[String] = """[a-zA-Z][\w.]*""".r
  def args: Parser[List[String]] = "(" ~> (repsep(identifier, ",") <~ ")")
  def multiArgs: Parser[List[List[String]]] = args.+

  def app: Parser[RawProperty] = identifier ~ multiArgs ^^ { (i, a) =>
    RawProperty.App(i, a)
  }

  def not: Parser[RawProperty.Not] = "!" ~> property ^^ { p =>
    RawProperty.Not(p)
  }

  def and: Parser[RawProperty] = {
    andProperty
  }
  
  def andToken: Parser[String] = "and" | "&&"

  def andProperty: Parser[RawProperty] = app ~ (andToken ~> andProperty).* ^^ {
    (p, ps) => ps.foldLeft(p) { (acc, r) =>
      RawProperty.And(acc, r)
    }
  }

  def orToken: Parser[String] = "or" | "||"

  def or: Parser[RawProperty] = and ~ (orToken ~> and).* ^^ {
    (p, ps) => ps.foldLeft(p) { (acc, r) =>
      RawProperty.Or(acc, r)
    }
  }

  def property: Parser[RawProperty] = ("("~> (property <~")")) | and | or | not

  def parse(input: String): ParseResult[RawProperty] = {
    parseAll(property, input)
  }
}


/** A property, i.e., an mCRL2 formula to verify [[effpi.verifier.mcrl2.Spec]]s. */
protected[mcrl2] abstract class Property {
  /** Verify the given LPS file, working in the given temporary dir.
    *
    * @param tmpdir Path to the directory for temporary files
    * @param lpsfile Path to the LPS file to be verified
    * @param spec The specification to be verified (should match `lpsfile`)
    * @param environment Verification environment, including probes
    * @param states Returns the number of states in the spec (for benchmarks)
    */
  def verify(tmpdir: Path, lpsfile: Path, spec: Spec,
             environment: Verifier.VerifEnv,
             states: () => String): Either[String, Boolean]

  /** The set of variables that are considered by this property. */
  val variables: Set[String]

  /** Return the mCRL2 formula as a string */
  def show: String
}

/** Main factory to instantiate property objects. */
object Property {
  /**
   * Base abstract class for all properties, implementing most functionality.
   *
   * @param shortName Short property name, used to select the property, and
   *                  select its `.mcf` file template.
   * @param description Property description.
   * @param observed Set of variables observed for I/O.
   * @param relied Set of variables allowed for I/O with external components.
   * @param observables Full set of observables.
   * @param extras Mapping from template arguments to sets of strings,
   *               representing variables with custom use (default: empty).
   *               E.g., `Map("foo" -> Set("bar", "baz"))` will expand the
   *               `$foo$` field (if present in the template `.mcf` file) into
   *               the mCRL2 data values for `bar`, `baz`.
   * @param options Verification options.
   * 
   * @note `observed`, and `relied` must correspond to sets of
   *       variables that are also subsets of `observables`. Instead, `extras`
   *       is not checked.
   */
  abstract case class Base(shortName: String,
                           description: String,
                           observed: Set[String],
                           relied: Set[String],
                           observables: Verifier.ObsEnv,
                           extras: Map[String, Set[String]],
                           options: Options)
                          (implicit ctx: Context) extends Property {
    import org.stringtemplate.v4.ST

    override val variables: Set[String] = observed ++ relied

    def runCommand(cmd: String,
                   args: Seq[String]): util.StdOutErrAndTime = {
      def logger(s: String) = report.log(s)
      util.runCommand(cmd, args, logger)
    }

    private lazy val tpl: String = {
      val res = "/mcrl2/" + shortName + ".mcf"
      report.log(s"Retrieving MCF file template from resource: ${res}")
      util.resource(res)
    }
    
    /** Generate the MCF formula, using `spec` to convert
      * observed and relied variables into corresponding mCRL2 data values.
      */
    private def formula(spec: Spec,
                        environment: Verifier.VerifEnv): Either[String, String] = {
      // Retrieve all approximated uses of observed variables, except mailboxes
      val obs_approx = (observables.filter {
          case TypeVar(name, widened) => observed.contains(name.toString)
          case _other => false
        } flatMap {
          // NOTE: the following cast is ugly, but safe (cf. filter above)
          tv => spec.approxUses(tv.asInstanceOf[TypeVar])
      }).mkString(", ")

      /* Rename a set of parameters into their mCRL2 representation, using the
       * `legend` of the mCRL2 spec, and skipping unknown parameters.
       * Also turn the result into a String, with comma-separated entries. */
      val legend = spec.legendHR
      report.log(s"Legend from human-readable names to mCRL2 aliases:\n${legend}")
      def legendize(args: Set[String]): String = {
        (args filter { legend.contains(_) } map { legend(_) }).mkString(", ")
      }

      // Collect all verification probes
      val syntVars = environment.filter {
        case v: effpi.verifier.SyntTypeVar => true
        case _ => false
      } map {
        case effpi.verifier.SyntTypeVar(name) => name
        case _ => throw new RuntimeException(s"BUG: should be unreachable!")
      }

      val observedAttr = legendize(observed)
      val reliedAttr = legendize(relied)
      val probesAttr = legendize(syntVars)
      val observedReliedAttr = legendize(observed ++ relied)
      val observedReliedProbesAttr = legendize(observed ++ relied ++ syntVars)
      val attrs = Map(
        "observed" -> observedAttr,
        "observed_approx" -> obs_approx,
        "relied" -> reliedAttr,
        "probes" -> probesAttr,
        "observed_relied" -> observedReliedAttr,
        "observed_relied_probes" -> observedReliedProbesAttr
      ) ++ extras.map { kv => kv._1 -> legendize(kv._2) }
      val res = attrs.foldLeft(ST(tpl, '$', '$')) { (acc, kv) =>
        acc.add(kv._1, kv._2)
      }.render
      report.log(s"Rendered MCF formula:\n${res}")
      Right(res)
    }

    private def mcfFile(tmpdir: Path,
                        spec: Spec,
                        environment: Verifier.VerifEnv): Either[String, Path] = {
      for { 
        fml <- formula(spec, environment)
      } yield {
        val path = tmpdir.resolve(Paths.get(shortName ++ ".mcf"))
        report.log(s"Creating MCF file: ${path.getFileName}")
        Files.write(path, fml.getBytes)
      }
    }

    private def pbesFile(tmpdir: Path,
                         lpsfile: Path,
                         spec: Spec,
                         environment: Verifier.VerifEnv): Either[String, Path] = {
      for {
        mcf <- mcfFile(tmpdir, spec, environment)
      } yield {
        val fname = lpsfile.getName(lpsfile.getNameCount - 1).toString
        val fnameNoExt = fname.substring(0, fname.lastIndexOf('.'))
        val path = tmpdir.resolve(Paths.get(s"${fnameNoExt}-${shortName}.pbes"))
        report.log(s"Creating PBES file: ${path.getFileName}")
        val cmd = "lps2pbes"
        val args = Seq("-f", s"${mcf}", s"${lpsfile}", s"${path}")
        runCommand(cmd, args)
        path
      }
    }

    // Return the mCRL2 command that verifies the parameterised boolean
    // equation system (PBES) of the given LPS file, for this property
    private def pbesCmdArgs(tmpdir: Path,
                            lpsfile: Path,
                            spec: Spec,
                            environment: Verifier.VerifEnv): Either[String, (String, Seq[String])] = {
      for {
        pbes <- pbesFile(tmpdir, lpsfile, spec, environment)
      } yield {
        options.solver match {
          case effpi.verifier.pbessolve => {
            ("pbessolve", Seq(s"${pbes}"))
          }
          case effpi.verifier.pbes2bool_depth => {
             ("pbes2bool", Seq("--strategy=2", "--search=depth-first", s"${pbes}"))
          }
          case effpi.verifier.pbes2bool_breadth => {
             ("pbes2bool", Seq("--strategy=2", "--search=breadth-first", s"${pbes}"))
          }
        }
      }
    }

    // Given the result of a pbes2bool invocation (via util.runCommand),
    // parse the process output and return the verification outcome (true/false)
    private def pbesResult(pt: util.StdOutErrAndTime): Boolean = {
      pt.stdout.split("\n")(0) match {
        case "true"  => true
        case "false" => false
        case _       => {
          throw new RuntimeException(s"Unexpected output: ${pt.stdout}")
        }
      }
    }

    override def verify(tmpdir: Path, lpsfile: Path, spec: Spec,
                        environment: Verifier.VerifEnv,
                        states: () => String): Either[String, Boolean] = {
      if (options.benchmarkReps == 0) { // No benchmarking, just verification
        for {
          cmdArgs <- pbesCmdArgs(tmpdir, lpsfile, spec, environment)
        } yield {
          report.log(s"Verifying property with legend:\n${spec.legendHR}")
          pbesResult(runCommand(cmdArgs._1, cmdArgs._2))
        }
      } else {
        assert(options.benchmarkReps >= 0)
        for {
          result <- benchmark(tmpdir, lpsfile, spec, environment, states)
        } yield {
          result
        }
      }
    }

    /** Benchmark verification time, with `benchmarkReps` repetitions. */
    private def benchmark(tmpdir: Path, lpsfile: Path, spec: Spec,
                          environment: Verifier.VerifEnv,
                          states: () => String): Either[String, Boolean] = for {
        cmdArgs <- pbesCmdArgs(tmpdir, lpsfile, spec, environment)
    } yield {
      report.warning(s"Benchmarking: ${options.specName}, ${shortName}. Please wait...")
      val (cmd, args) = cmdArgs
      
      val bench: Any => (Boolean, Long) = { _ =>
        val ret = runCommand(cmd, args)
        (pbesResult(ret), ret.nanosecs)
      }
      
      val results = (1 to options.benchmarkReps).map { n => 
        report.log(s"Benchmarking: run ${n} of ${options.benchmarkReps}")
        bench(n)
      }

      assert(results.length >= 1)
      // Just out of paranoia, ensure that all results are the same...
      assert(results.map(_._1).toSet.size == 1)
      // ...then pick the first one as verification outcome
      val result = results(0)._1 

      val output = tmpdir.resolve(Paths.get(s"benchmark-${options.specName}-${shortName}.csv"))
      report.warning(s"Writing benchmark results on: ${output}", options.position)
      val csv = Seq("result,states,nanosecs") ++ results.map { (r, t) =>
        s"${r},${states()},${t}"
      } ++ Seq("") // Empty element, just to add a final "\n" (see next line)
      Files.write(output, csv.mkString("\n").getBytes)

      result
    }

    /** Return the property formula as a string. */
    def show: String = tpl
  }

  /** Internal companion object for creating base properties. */
  protected[mcrl2] object Base {
    /** For the meaning of the arguments, see [[Base]] class above. */
    def apply(shortName: String,
              description: String,
              observed: Set[String],
              relied: Set[String],
              observables: Verifier.ObsEnv,
              options: Options,
              extras: Map[String, Set[String]] = Map())
             (implicit ctx: Context): Either[String, Property] = {
      // Ensure that `observables` contains everything that the property
      // is trying to observe or rely upon
      def isObservable(x: String): Boolean = {
        (x == Spec.MAILBOX_ALIAS) || (observables.filter {
          case TypeVar(name, _) => (name.toString == x)
          case _other => false
        }).size > 0
      }
      def obsFold(acc: Set[String], x: String): Set[String] = {
        if (isObservable(x)) acc else acc + x
      }
      val unknownObs = (observed ++ relied).foldLeft(Set[String]())(obsFold)
      if (!unknownObs.isEmpty) {
        val unknStr = unknownObs.mkString(", ")
        Left(s"Property refers to an unknow observable: ${unknStr}")
      } else {
        Right(new Base(shortName, description, observed, relied, observables,
                       extras, options) {})
      }
    }
  }

  case class And(p1: Property, p2: Property)
                (options: Options) extends Property {
    if (options.benchmarkReps > 0) {
      throw new UnsupportedOperationException("TODO: Benchmarking is not yet supported for composed properties")
    }
    override def verify(tmpdir: Path, lpsfile: Path, spec: Spec,
                        environment: Verifier.VerifEnv,
                        states: () => String): Either[String, Boolean] = for {
      res1 <- p1.verify(tmpdir, lpsfile, spec, environment, states)
      res2 <- {
        if (res1) p2.verify(tmpdir, lpsfile, spec, environment, states)
        else Right(false)
      }
    } yield res1 && res2

    override val variables: Set[String] = p1.variables ++ p2.variables

    override def show: String = s"(${p1.show}) and (${p2.show})"
  }

  case class Or(p1: Property, p2: Property)
               (options: Options) extends Property {
    if (options.benchmarkReps > 0) {
      throw new UnsupportedOperationException("TODO: Benchmarking is not yet supported for composed properties")
    }
    override def verify(tmpdir: Path, lpsfile: Path, spec: Spec,
                        environment: Verifier.VerifEnv,
                        states: () => String): Either[String, Boolean] = for {
      res1 <- p1.verify(tmpdir, lpsfile, spec, environment, states)
      res2 <- {
        if (!res1) p2.verify(tmpdir, lpsfile, spec, environment, states)
        else Right(false)
      }
    } yield res1 || res2

    override val variables: Set[String] = p1.variables ++ p2.variables

    override def show: String = s"(${p1.show}) or (${p2.show})"
  }

  case class Not(p: Property)
                (options: Options) extends Property {
    override def verify(tmpdir: Path, lpsfile: Path, spec: Spec,
               environment: Verifier.VerifEnv,
               states: () => String): Either[String, Boolean] = for {
      res <- p.verify(tmpdir, lpsfile, spec, environment, states)
    } yield !res

    override val variables: Set[String] = p.variables

    override def show: String = s"!(${p.show})"
  }

  /** Try to parse a property from the given string. */
  def apply(input: String,
            observables: Verifier.ObsEnv,
            options: Options)
            (implicit ctx: Context): Either[String, Property] = {        
    PropertyParser.parse(input) match {
      case PropertyParser.NoSuccess(msg, input) => {
        Left(parseError(input.pos, msg))
      }
      case PropertyParser.Success(res, _next) => res match {
        case rp: RawProperty => apply(rp, observables, options)
      }
    }
  }

  /** Try to instantiate a property from a RawProperty. */
  private def apply(input: RawProperty,
                    observables: Verifier.ObsEnv,
                    options: Options)
           (implicit ctx: Context): Either[String, Property] = input match {
    case RawProperty.And(rp1, rp2) => for {
      p1 <- apply(rp1, observables, options)
      p2 <- apply(rp2, observables, options)
    } yield And(p1, p2)(options)
    case RawProperty.Or(rp1, rp2) => for {
      p1 <- apply(rp1, observables, options)
      p2 <- apply(rp2, observables, options)
    } yield Or(p1, p2)(options)
    case RawProperty.Not(rp) => for {
      p <- apply(rp, observables, options)
    } yield Not(p)(options)
    case rp: RawProperty.App => {
      report.log(s"Selecting property ${rp.name} from:\n${BaseProperties.all}")
      val prop = BaseProperties.all.filter { _.name == rp.name }
      if (prop.size == 0) {
        Left(s"Unknown property: ${rp.name}")
      } else {
        assert(prop.size == 1)
        prop.last(rp, observables, options)
      }
    }
  }

  /** Format parsing error as a readable string */
  private def parseError(pos: scala.util.parsing.input.Position,
                         msg: String): String = {
    s"Parsing error on line ${pos.line}, column ${pos.column}:\n" ++
      s"${pos.longString}\n${msg}"
  }
}

/** A container for the properties available on effpi. */
object BaseProperties {
  /** The set of all known properties */
  val all: Set[PropertyObj] = SortedSet[PropertyObj](
    Responsiveness, Reactiveness, DeadlockFreedom,
    NoOutputUse, EventualOutputUse, Forwarding, OutputFollowed
  )(Ordering.by(_.description))
}

/** Base class of all (companion) objects that might produce properties. */
private abstract case class PropertyObj(name: String, description: String) {
  def apply(input: RawProperty.App,
            observables: Verifier.ObsEnv,
            options: Options)
           (implicit ctx: Context): Either[String, Property]
}

/** Ensure that whenever a channel is received from an observed variable.
 *   it is used to send a response.
 */
private object Responsiveness extends PropertyObj("responsive",
                                                  "Responsiveness") {
  override def apply(input: RawProperty.App,
                     observables: Verifier.ObsEnv,
                     options: Options)
                    (implicit ctx: Context): Either[String, Property] = {
    if (input.args.length < 1 || input.args.length > 2) {
      return Left(s"Usage: ${name}(obs1, ...)[(rely1, ...)]")
    }
    val rely = if (input.args.length == 2) Set(input.args(1):_*) else Set()
    Property.Base(name, description, Set(input.args(0):_*), rely, observables,
                  options)
  }
}

/** Ensure that some observed variable is always eventually used for input. */
private object Reactiveness extends PropertyObj("reactive",
                                                "Reactiveness") {
  override def apply(input: RawProperty.App,
                     observables: Verifier.ObsEnv,
                     options: Options)
                    (implicit ctx: Context): Either[String, Property] = {
    if (input.args.length < 1 || input.args.length > 2) {
      return Left(s"Usage: ${name}(obs1, ...)[(rely1, ...)]")
    }
    val rely = if (input.args.length == 2) Set(input.args(1):_*) else Set()
    Property.Base(name, description, Set(input.args(0):_*), rely,
                  observables, options)
  }
}

/** Deadlock-freedom property. */
private object DeadlockFreedom extends PropertyObj("deadlock_free",
                                                   "Deadlock-free") {
  override def apply(input: RawProperty.App,
                     observables: Verifier.ObsEnv,
                     options: Options)
                    (implicit ctx: Context): Either[String, Property] = {
    if (input.args.length < 1 || input.args.length > 1) {
      return Left(s"Usage: ${name}(rely1, ...)")
    }
    val rely = Set(input.args(0):_*)
    Property.Base(name, description, Set(), rely, observables, options)
  }
}

/** Ensure that no observed variable is used for output. */
private object NoOutputUse extends PropertyObj("no_output_use",
                                               "No output use") {
  override def apply(input: RawProperty.App,
                     observables: Verifier.ObsEnv,
                     options: Options)
                    (implicit ctx: Context): Either[String, Property] = {
    if (input.args.length < 1 ||
        // FIXME: we allow an extra empty argument list, for backwards compat.
        (input.args.length >= 2 && (input.args(1).length > 0))) {
      return Left(s"Usage: ${name}(obs1, ...)")
    }
    val observed = Set(input.args(0):_*)
    if (observed.size == 0) {
      return Left(s"Property '${name}' needs at least one variable to observe")
    }
    Property.Base(name, description, observed, Set(), observables, options)
  }
}

/** Ensure that an observed variable is always eventually used for output. */
private object EventualOutputUse extends PropertyObj("eventual_output_use",
                                                     "Eventual output use") {
  override def apply(input: RawProperty.App,
                     observables: Verifier.ObsEnv,
                     options: Options)
                    (implicit ctx: Context): Either[String, Property] = {
    if (input.args.length < 1 || input.args.length > 2) {
      return Left(s"Usage: ${name}(obs1, ...)[(rely1, ...)]")
    }
    val observed = Set(input.args(0):_*)
    if (observed.size == 0) {
      return Left(s"Property '${name}' needs at least one variable to observe")
    }
    val rely = if (input.args.length == 2) Set(input.args(1):_*) else Set()
    Property.Base(name, description, observed, rely, observables, options)
  }
}

/** Ensure that each input from an observed variable is always eventually
  * forwarded through a selected channel. */
private object Forwarding extends PropertyObj("forwarding",
                                              "Forwarding of received values") {
  override def apply(input: RawProperty.App,
                     observables: Verifier.ObsEnv,
                     options: Options)
                    (implicit ctx: Context): Either[String, Property] = {
    if (input.args.length < 2 || input.args.length > 3) {
      return Left(s"Usage: ${name}(obs1, ...)(fwd1, ...)[(rely1, ...)]")
    }
    val observed = Set(input.args(0):_*)
    if (observed.size == 0) {
      return Left(s"Property '${name}' needs at least one variable to observe")
    }
    val forwardTo = Set(input.args(1):_*)
    if (forwardTo.size == 0) {
      return Left(s"Property '${name}' needs at least one variable to forward to")
    }
    val rely = if (input.args.length == 3) Set(input.args(2):_*) else Set()
    Property.Base(name, description, observed, rely, observables, options,
                  Map("forwarding_targets" -> forwardTo))
  }
}

/** Ensure that an output on an observed variable is always eventually followed
  * by an input on a selected channel, with a given payload. */
private object OutputFollowed extends PropertyObj("output_eventually_followed",
                                                  "An output is followed by another") {
  override def apply(input: RawProperty.App,
                     observables: Verifier.ObsEnv,
                     options: Options)
                    (implicit ctx: Context): Either[String, Property] = {
    if (input.args.length < 2 || input.args.length > 3) {
      return Left(s"Usage: ${name}(obs1, ...)(payload1, ...)[(rely1, ...)]")
    }
    val observed = Set(input.args(0):_*)
    if (observed.size == 0) {
      return Left(s"Property '${name}' needs at least one variable to observe")
    }
    val payloads = Set(input.args(1):_*)
    if (payloads.size == 0) {
      return Left(s"Property '${name}' needs at least one payload for follow-up output")
    }
    val rely = if (input.args.length == 3) Set(input.args(2):_*) else Set()
    Property.Base(name, description, observed, rely, observables, options,
                  Map("followup_payloads" -> payloads))
  }
}
