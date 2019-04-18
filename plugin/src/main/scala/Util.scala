// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.verifier

import java.io.{BufferedReader, InputStreamReader}

package object util {
  /** Convert a list of options into an optional list */
  def optList[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case hd :: tl => for { x <- hd; l <- optList(tl) } yield x :: l
    case Nil => Some(Nil)
  }

  /** Return the contents of a resource as a String */
  def resource(name: String): String = {
    val cls = effpi.verifier.Verifier.getClass
    val stream = cls.getResourceAsStream(name)
    assert(stream != null)
    val res = scala.io.Source.fromInputStream(stream).getLines.mkString("\n")
    stream.close()
    res
  }

  /** Used to store the standard output of a process launched via
    * external command, and its execution time */
  case class StdOutErrAndTime(stdout: String, stderr: String, nanosecs: Long)
  
  /** Run a system command, and return its process handle, and execution
    * time in nsecs.
    * 
    *  @throws java.lang.RuntimeException if command's return status is not 0
    */
  def runCommand(cmd: String, args: scala.Seq[String],
                 logger: String => Unit = _ => ()): StdOutErrAndTime = {
    logger(s"""Executing: ${cmd} ${args.mkString(" ")}""")
    val pbargs: java.util.List[String] = {
      val lst = new java.util.ArrayList[String]()
      List((cmd +: args):_*).foreach { lst.add(_) }
      lst
    }
    val builder = new ProcessBuilder(pbargs)
    val startTime: Long = System.nanoTime()
    val p = builder.start()

    val outReader = new BufferedReader(new InputStreamReader(p.getInputStream))
    val outStr = {
      Iterator.continually(outReader.readLine()).takeWhile(_ != null).mkString("\n")
    }

    val errReader = new BufferedReader(new InputStreamReader(p.getErrorStream))
    val errStr = {
      Iterator.continually(errReader.readLine()).takeWhile(_ != null).mkString("\n")
    }

    val r = p.waitFor()
    val endTime: Long = System.nanoTime()

    if (r != 0) {
      throw new RuntimeException(
        s"""Command failed with code ${r}: ${cmd} ${args.mkString(" ")}"""
          + s"\nStandard error:\n" + errStr)
    }
    StdOutErrAndTime(outStr, errStr, endTime - startTime)
  }
}
