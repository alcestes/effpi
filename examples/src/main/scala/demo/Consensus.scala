package effpi.examples.demo.consensus

import scala.concurrent.duration.Duration

import effpi.process._
import effpi.process.dsl._
import effpi.channel.{Channel, InChannel, OutChannel}

case class A()
case class B()
case class Timeout()
case class Error()

case class Msg[Id <: Int, V <: A|B](sender: Id, payload: V)

type Agent[Id <: Int, Init <: A|B, Ci <: InChannel[Msg[?,?]],
           Co1 <: OutChannel[Msg[?,?]], 
           Co2 <: OutChannel[Msg[?,?]],
           Co3 <: OutChannel[Msg[?,?]]] = (
  Broadcast[Id, Init, Co1, Co2, Co3] >>:
  Rec[RecX,
      In[Ci, Msg[?,?], (v1: Msg[?,?]) =>
         In[Ci, Msg[?,?], (v2: Msg[?,?]) =>
            In[Ci, Msg[?,?], (v3: Msg[?,?]) =>
               Phase[Id, v1.payload.type, v2.payload.type, v3.payload.type,
                     Co1, Co2, Co3]]]]]
)

type Phase[Id <: Int, V1 <: A|B, V2 <: A|B, V3 <: A|B,
           Co1 <: OutChannel[Msg[?,?]],
           Co2 <: OutChannel[Msg[?,?]],
           Co3 <: OutChannel[Msg[?,?]]] <: Process = V1 match {
  case A => PhaseA[Id, V2, V3, Co1, Co2, Co3]
  case B => PhaseB[Id, V2, V3, Co1, Co2, Co3]
}

type PhaseA[Id <: Int, V2 <: A|B, V3 <: A|B,
            Co1 <: OutChannel[Msg[?,?]],
            Co2 <: OutChannel[Msg[?,?]],
            Co3 <: OutChannel[Msg[?,?]]] <: Process = V2 match {
  case A => PhaseAA[Id, V3, Co1, Co2, Co3]
  case B => PhaseAB[Id, V3, Co1, Co2, Co3]
}

type PhaseAA[Id <: Int, V3 <: A|B,
             Co1 <: OutChannel[Msg[?,?]],
             Co2 <: OutChannel[Msg[?,?]],
             Co3 <: OutChannel[Msg[?,?]]] <: Process = V3 match {
  case A => PNil // Consensus reached
  case B => Broadcast[Id, A, Co1, Co2, Co3] >>: Loop[RecX]
}

type PhaseAB[Id <: Int, V3 <: A|B,
             Co1 <: OutChannel[Msg[?,?]],
             Co2 <: OutChannel[Msg[?,?]],
             Co3 <: OutChannel[Msg[?,?]]] <: Process = V3 match {
  case A => Broadcast[Id, A, Co1, Co2, Co3] >>: Loop[RecX]
  case B => Broadcast[Id, B, Co1, Co2, Co3] >>: Loop[RecX]
}

type PhaseB[Id <: Int, V2 <: A|B, V3 <: A|B,
            Co1 <: OutChannel[Msg[?,?]],
            Co2 <: OutChannel[Msg[?,?]],
            Co3 <: OutChannel[Msg[?,?]]] <: Process = V2 match {
  case A => PhaseBA[Id, V3, Co1, Co2, Co3]
  case B => PhaseBB[Id, V3, Co1, Co2, Co3]
}

type PhaseBA[Id <: Int, V3 <: A|B,
             Co1 <: OutChannel[Msg[?,?]],
             Co2 <: OutChannel[Msg[?,?]],
             Co3 <: OutChannel[Msg[?,?]]] <: Process = V3 match {
  case A => Broadcast[Id, A, Co1, Co2, Co3] >>: Loop[RecX]
  case B => Broadcast[Id, B, Co1, Co2, Co3] >>: Loop[RecX]
}

type PhaseBB[Id <: Int, V3 <: A|B,
             Co1 <: OutChannel[Msg[?,?]],
             Co2 <: OutChannel[Msg[?,?]],
             Co3 <: OutChannel[Msg[?,?]]] <: Process = V3 match {
  case A => Broadcast[Id, B, Co1, Co2, Co3] >>: Loop[RecX]
  case B => PNil // Consensus reached
}

type Broadcast[Id <: Int, V <: A|B,
               Co1 <: OutChannel[Msg[?,?]],
               Co2 <: OutChannel[Msg[?,?]],
               Co3 <: OutChannel[Msg[?,?]]] = (
  Out[Co1, Msg[Id,V]] >>: Out[Co2, Msg[Id,V]] >>: Out[Co3, Msg[Id,V]]
)

type System[Init1 <: A|B, Init2 <: A|B, Init3 <: A|B,
            C1 <: Channel[Msg[?,?]],
            C2 <: Channel[Msg[?,?]],
            C3 <: Channel[Msg[?,?]]] = (
  Par3[
    Agent[1, Init1, C1, C1, C2, C3],
    Agent[2, Init2, C2, C1, C2, C3],
    Agent[3, Init3, C3, C1, C2, C3]
  ]
)

def broadcast[V <: A|B](id: Int, v: V,
              c1: OutChannel[Msg[?,?]],
              c2: OutChannel[Msg[?,?]],
              c3: OutChannel[Msg[?,?]]): Broadcast[id.type, V, c1.type, c2.type, c3.type] = {
  send(c1, Msg(id, v)) >> {
    send(c2, Msg(id, v)) >>
    send(c3, Msg(id, v))
  }
}

def agent(id: Int, init: A|B, ci: InChannel[Msg[?,?]],
          co1: OutChannel[Msg[?,?]], co2: OutChannel[Msg[?,?]],co3: OutChannel[Msg[?,?]])
         (implicit t: Duration): Agent[id.type, init.type, ci.type,
                                       co1.type, co2.type, co3.type] = {
  broadcast(id, init, co1, co2, co3) >>
  rec(RecX) {
    receive(ci) { v1 =>
      receive(ci) { v2 =>
        receive(ci) { v3 =>
          phase(id, v1.payload, v2.payload, v3.payload, co1, co2, co3)
        }
      }
    }
  }
}

def phase(id: Int, v1: A|B, v2: A|B, v3: A|B,
          c1: OutChannel[Msg[?,?]],
          c2: OutChannel[Msg[?,?]],
          c3: OutChannel[Msg[?,?]]): Phase[id.type, v1.type, v2.type, v3.type,
                                           c1.type, c2.type, c3.type] = {
  v1 match {
    case _: A => v2 match {
      case _: A => v3 match {
        case _: A => nil // Consensus reached
        case _: B => broadcast(id, A(), c1, c2, c3) >> loop(RecX)
      }
      case _: B => v3 match {
        case _: A => broadcast(id, A(), c1, c2, c3) >> loop(RecX)
        case _: B => broadcast(id, B(), c1, c2, c3) >> loop(RecX)
      }
    }
    case _: B => v2 match {
      case _: A => v3 match {
        case _: A => broadcast(id, A(), c1, c2, c3) >> loop(RecX)
        case _: B => broadcast(id, B(), c1, c2, c3) >> loop(RecX)
      }
      case _: B => v3 match {
        case _: A => broadcast(id, B(), c1, c2, c3) >> loop(RecX)
        case _: B => nil // Consensus reached
      }
    }
  }
}

/*
def system(c11: Channel[A|B], c12: Channel[A|B], c13: Channel[A|B],
           c21: Channel[A|B], c22: Channel[A|B], c23: Channel[A|B],
           c31: Channel[A|B], c32: Channel[A|B], c33: Channel[A|B])
          (implicit timeout: Duration): System[A|B, A|B, A|B,
                                               c11.type, c12.type, c13.type,
                                               c21.type, c22.type, c23.type,
                                               c31.type, c32.type, c33.type] = {
  par(
    agent(A(), c11, c12, c13, c11, c21, c31),
    agent(B(), c21, c22, c23, c12, c22, c32),
    agent(A(), c31, c32, c33, c13, c23, c33)
  )
}
*/