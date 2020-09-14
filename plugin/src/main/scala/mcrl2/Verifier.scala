// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.verifier.mcrl2

import effpi.verifier.{Options, util}

import scala.language.implicitConversions

import java.io.{BufferedReader, InputStreamReader}
import java.nio.file.{Files, Path, Paths}

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report

/** Playground to verify whether a set of [[mpstk.mcrl2.Spec]]s satisfy a
  * set of [[mpstk.mcrl2.Property]]. */
protected[mcrl2]
class Verifier(val spec: Spec,
               val property: Property,
               options: Options)
              (implicit ctx: Context) { // extends LazyLogging {
  def runCommand(cmd: String,
                 args: Seq[String]): util.StdOutErrAndTime = {
    def logger(s: String) = report.log(s)
    util.runCommand(cmd, args, logger)
  }
  
  lazy val tempDir: Path = {
    val dir = Files.createTempDirectory("effpi-")
    report.log(s"New temporary directory: ${dir}")
    dir
  }

  private
  lazy val specFile: Path = {
    val path = tempDir.resolve(Paths.get(spec.filename ++ ".mcrl2"))
    report.log(s"Creating mCRL2 specification file: ${path}")
    Files.write(path, spec.show.getBytes)
  }

  private
  lazy val lpsFile: Path = {
    val path = tempDir.resolve(Paths.get(spec.filename ++ ".lps"))
    report.log(s"Generating LPS file: ${path}")
    val cmd = "mcrl22lps"
    val args = Seq(s"${specFile}", s"${path}")
    runCommand(cmd, args)
    path
  }

  private
  lazy val ltsFile: Path = {
    val path = tempDir.resolve(Paths.get(spec.filename ++ ".lts"))
    report.warning(s"Generating LTS file: ${path}. This might take a while...")
    val cmd = "lps2lts"
    val args = Seq(s"${lpsFile}", s"${path}")
    runCommand(cmd, args)
    path
  }

  /** The number of states of the mCRL2 specification {@code spec}, or "∞"
    * if `isBigLTS` is `true`
    */
  lazy val states: String = {
    if (options.isBigLTS) "∞" else {
      ltsinfoResult(runCommand("ltsinfo", Seq(s"${ltsFile}"))).toString
    }
  }

  /** Close the verifier.
    *
    * This method disposes of all the verifier resources (e.g., its
    * temporary files), unless `keepTmp` is `true`.
    * After invoking this method, the verifier behaviour is undefined.
    */
  def close(): Unit = if (options.keepTmp) {
    report.log(s"NOT removing temporary dir: ${tempDir}")
  } else {
    report.log(s"Removing temporary dir: ${tempDir}")
    deleteDir(tempDir.toFile)
  } 

  // Given the result of an ltsinfo invocation (via util.runCommand),
  // parse the process output and return the number of states
  private def ltsinfoResult(pt: util.StdOutErrAndTime): Long = {
    val res = pt.stderr.split("\n")(0) // Only take the first line of output
    val nstatesRegex = raw"Number of states: (\d+)\.$$".r
    res match {
      case nstatesRegex(nstates) => nstates.toLong
    }
  }

  // Remove the given directory with all its files and sub-directories
  private def deleteDir(d: java.io.File): Unit = {
    assert(d.isDirectory && d.canWrite)

    val dirs = d.listFiles.filter(_.isDirectory)
    for { dir <- dirs } deleteDir(dir)

    val files = d.listFiles.filter(_.isFile)
    assert(files.forall(_.canWrite))

    val delresult = for { f <- files } yield f.delete()
    assert(delresult.forall(x => x))

    d.delete()
  }

  /** Check whether the spec satisfies the property. */
  def verify(env: effpi.verifier.Verifier.VerifEnv): Either[String, Boolean] = {
    property.verify(tempDir, lpsFile, spec, env, () => states)
  }
  
  override val toString: String = s"Verifier(${spec.description})"
}

object Verifier {
  /** Create a verifier that checks whether {@code spec} satisfies
    * the given {@code property}.
    */
  def apply(spec: Spec, property: Property, options: Options)
           (implicit ctx: Context): Verifier = {
    new Verifier(spec, property, options)
  }
}
