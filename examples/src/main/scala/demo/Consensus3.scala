package effpi.examples.demo.consensus3

import scala.concurrent.duration.Duration
// import scala.compiletime.S
// import scala.compiletime.ops.int._

import effpi.process._
import effpi.process.dsl._
import effpi.channel.{Channel, InChannel, OutChannel}

import effpi.examples.demo.refinement._

case class A()
case class B()
case class Timeout()
case class Error()

case class Msg[Id <: Int, V <: A|B](sender: Id, payload: V)

type Agent[Id <: Int, Init <: A|B, Ci <: InChannel[Msg[?,?]],
           Peers <: Seq[OutChannel[Msg[?,?]]]] = (
  Broadcast[Id, Init, Peers] >>:
  Rec[RecX,
    Phase[Id, Ci, Peers, 0, 0, 0]
  ]
)

/* To check:
type Password[...] = In(C, Passwd, (pwd: Passwd) => (
  Out[C, "Please Retry"] >> Loop(RecX) |
  ( pwd.type.match {
    case Correct => ...
    case Incorrect => ...
  )
})
*/

type Phase[Id <: Int, Ci <: InChannel[Msg[?,?]],
           Peers <: Seq[OutChannel[Msg[?,?]]],
           N <: Nat, As <: Nat, Bs <: Nat] <: Process = Compare[Size[Peers], N] match {
  case Greater[?,?] => EndPhase[Id, Ci, Peers, As, Bs]
  case LessEqual[?,?] => In[Ci, Msg[?,?], (msg: Msg[?,?]) =>
                            MsgMatch[msg.payload.type,
                                     Id, Ci, Peers, N, As, Bs]]
}

type MsgMatch[Cmp <: Compare2[?,?], Id <: Int, Ci <: InChannel[Msg[?,?]],
              Peers <: Seq[OutChannel[Msg[?,?]]],
              N <: Nat, As <: Nat, Bs <: Nat] <: Process = Cmp match {
    case A => Phase[Id, Ci, Peers, Succ[N], Succ[As], Bs]
    case B => Phase[Id, Ci, Peers, Succ[N], As, Succ[Bs]]
}

type EndPhase[Id <: Int, Ci <: InChannel[Msg[?,?]],
              Peers <: Seq[OutChannel[Msg[?,?]]],
              As <: Nat, Bs <: Nat] <: Process = Compare[As, Bs] match {
  case Greater[?,?] =>
    Broadcast[Id, A, Peers] >>: Loop[RecX]
  case LessEqual[?,?] =>
    Broadcast[Id, B, Peers] >>: Loop[RecX]
}

type Broadcast[Id <: Int, V <: A|B, Peers <: Seq[OutChannel[Msg[?,?]]]] = (
  Foreach[Peers, OutChannel[Msg[?,?]], (c: OutChannel[Msg[?,?]]) =>
    Out[c.type, Msg[Id, V]]
  ]
)

def broadcast[Id <: Int, V <: A|B,
              Cs <: Seq[OutChannel[Msg[?,?]]]](id: Id, v: V,
                                               cs: Cs): Broadcast[Id, V, Cs] = {
  foreach(cs) { c =>
    send(c, Msg(id, v))
  }
}

def endPhase[Id <: Int, Ci <: InChannel[Msg[?,?]], NA <: Nat, NB <: Nat, Peers <: Seq[OutChannel[Msg[?,?]]]]
            (id: Id, ci: Ci, peers: Peers, nA: NA, nB: NB): EndPhase[Id, Ci, Peers, NA, NB] = {
  compare(nA, nB) match {
    case _: Greater[?,?] => broadcast(id, A(), peers) >> loop(RecX)
    case _: LessEqual[?,?] => broadcast(id, B(), peers) >> loop(RecX)
  }
}

/** Does not compile due to Dotty bug #9999 :-( */
/*
def phase[Id <: Int, Ci <: InChannel[Msg[?,?]], N <: Nat, NA <: Nat, NB <: Nat, Peers <: Seq[OutChannel[Msg[?,?]]]]
         (id: Id, ci: Ci, peers: Peers, n: N, nA: NA, nB: NB)
         (implicit timeout: Duration): Phase[Id, Ci, Peers,
                                             N, NA, NB] = compare(size(peers), n) match {
  case _: Greater[?,?] => endPhase(id, ci, peers, nA, nB)
  case _: LessEqual[?,?] => receive(ci) { msg => msg.payload match {
    case _: A => phase(id, ci, peers, succ(n), succ(nA), nB)
    case _: B => phase(id, ci, peers, succ(n), nA, succ(nB))
  } }
}
*/