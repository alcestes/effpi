package effpi.examples.demo.let

import scala.concurrent.duration.Duration
// import scala.compiletime.S
// import scala.compiletime.ops.int._

import effpi.process._
import effpi.process.dsl._
import effpi.channel.{Channel, InChannel, OutChannel}

import effpi.examples.demo.refinement._

type LetExample[N <: Nat, C <: OutChannel[Nat]] = Let[Nat, (n: Nat) => LetMatch[N, n.type, C]]

type LetMatch[N <: Nat, M <: Nat, C <: OutChannel[Nat]] <: Process = Compare[M, N] match {
  case Greater[?,?] => Out[C, M]
  case LessEqual[?,?] => Out[C, N]
}

def letExample(n: Nat, c: OutChannel[Nat]): LetExample[n.type, c.type] = {
  let(42) { (x: Nat) =>
    compare(x, n) match {
      case _: Greater[?,?] => send(c, x)
      case _: LessEqual[?,?] => send(c, n)
    }
  }
}
