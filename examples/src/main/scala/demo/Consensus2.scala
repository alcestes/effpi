package effpi.examples.demo.consensus2

import scala.concurrent.duration.Duration
import scala.compiletime.S
import scala.compiletime.ops.int._

import effpi.process._
import effpi.process.dsl._
import effpi.channel.{Channel, InChannel, OutChannel}

case class A()
case class B()
case class Timeout()
case class Error()

case class Msg[Id <: Int, V <: A|B](sender: Id, payload: V)

type Agent[Id <: Int, Init <: A|B, Ci <: InChannel[Msg[?,?]],
           Peers <: Seq[OutChannel[Msg[?,?]]], NPeers <: Int] = (
  Broadcast[Id, Init, Peers] >>:
  Rec[RecX,
    Phase[Id, Ci, Peers, NPeers, NPeers, 0, 0]
  ]
)

type Phase[Id <: Int, Ci <: InChannel[Msg[?,?]],
           Peers <: Seq[OutChannel[Msg[?,?]]], NPeers <: Int,
           N <: Int, As <: Int, Bs <: Int] <: Process = N match {
  case 0 => EndPhase[Id, Ci, Peers, NPeers, As, Bs]
  case S[m] => In[Ci, Msg[?,?], (msg: Msg[?,?]) =>
                    MsgMatch[msg.sender.type, msg.payload.type,
                             Id, Ci, Peers, NPeers, m, As, Bs]]
}

type MsgMatch[MSender <: Int, MPayload <: A|B, Id <: Int, Ci <: InChannel[Msg[?,?]],
              Peers <: Seq[OutChannel[Msg[?,?]]], NPeers <: Int,
              N <: Int, As <: Int, Bs <: Int] <: Process = MPayload match {
    case A => Phase[Id, Ci, Peers, NPeers, N, As+1, Bs]
    case B => Phase[Id, Ci, Peers, NPeers, N, As, Bs+1]
}

type EndPhase[Id <: Int, Ci <: InChannel[Msg[?,?]],
              Peers <: Seq[OutChannel[Msg[?,?]]],
              NPeers <: Int, As <: Int, Bs <: Int] <: Process = As <= Bs match {
  case false =>
    Broadcast[Id, A, Peers]// >>: Phase[Id, Ci, Peers, NPeers, NPeers, 0, 0]
  case true =>
    Broadcast[Id, B, Peers]// >>: Phase[Id, Ci, Peers, NPeers, NPeers, 0, 0]
}

type Broadcast[Id <: Int, V <: A|B, Peers <: Seq[OutChannel[Msg[?,?]]]] = (
  Foreach[Peers, OutChannel[Msg[?,?]], (c: OutChannel[Msg[?,?]]) =>
    Out[c.type, Msg[Id, V]]
  ]
)

def broadcast[V <: A|B](id: Int, v: V,
                        cs: Seq[OutChannel[Msg[?,?]]]): Broadcast[id.type, V, cs.type] = {
  foreach(cs) { c =>
    send(c, Msg(id, v))
  }
}

/* The following does not compile because (nA <: nB) has type Boolean,
 * instead of (nA.type <= nB.type)

def endPhase(id: Int, ci: InChannel[Msg[?,?]], peers: Seq[OutChannel[Msg[?,?]]],
             nPeers: Int, nA: Int, nB: Int): EndPhase[id.type, ci.type,
                                                      peers.type, nPeers.type,
                                                      nA.type, nB.type] = ((nA <= nB): (nA.type <= nB.type)) match {
  case _: true => broadcast(id, B(), peers)
  case _: false => broadcast(id, A(), peers)
}
*/

def phase2(id: Int, ci: InChannel[Msg[?,?]],
           peers: Seq[OutChannel[Msg[?,?]]])
          (implicit timeout: Duration): Phase[id.type, ci.type, peers.type,
                                              2, 2, 0, 0] = {
  receive(ci) { msg => msg.payload match {
    case _: A => receive (ci) { msg2 => msg2.payload match {
      case _: A => broadcast(id, A(), peers)
      case _: B => broadcast(id, B(), peers)
    } }
    case _: B => receive (ci) { msg2 => msg2.payload match {
      case _: A => broadcast(id, B(), peers)
      case _: B => broadcast(id, B(), peers)
    } }
  } }
}

def phase3(id: Int, ci: InChannel[Msg[?,?]],
           peers: Seq[OutChannel[Msg[?,?]]])
          (implicit timeout: Duration): Phase[id.type, ci.type, peers.type,
                                              3, 3, 0, 0] = {
  receive(ci) { msg => msg.payload match {
    case _: A => receive (ci) { msg2 => msg2.payload match {
      case _: A => receive (ci) { msg3 => msg3.payload match {
        case _: A => broadcast(id, A(), peers)
        case _: B => broadcast(id, A(), peers)
      } }
      case _: B => receive (ci) { msg3 => msg3.payload match {
        case _: A => broadcast(id, A(), peers)
        case _: B => broadcast(id, B(), peers)
      } }
    } }
    case _: B => receive (ci) { msg2 => msg2.payload match {
      case _: A => receive (ci) { msg3 => msg3.payload match {
        case _: A => broadcast(id, A(), peers)
        case _: B => broadcast(id, B(), peers)
      } }
      case _: B => receive (ci) { msg3 => msg3.payload match {
        case _: A => broadcast(id, B(), peers)
        case _: B => broadcast(id, B(), peers)
      } }
    } }
  } }
}
