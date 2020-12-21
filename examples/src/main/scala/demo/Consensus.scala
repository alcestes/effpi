package effpi.examples.demo.consensus

import scala.concurrent.duration.Duration

import effpi.process._
import effpi.process.dsl._
import effpi.channel.{Channel, InChannel, OutChannel}

case class A();
case class B();

type Agent[Init <: A|B,
           C1 <: Channel[A|B],
           C2 <: Channel[A|B],
           C3 <: Channel[A|B]] = (
  Broadcast[Init, C1, C2, C3] >>:
  Rec[RecX,
      In[C1, A|B, (v1:A|B) =>
         In[C2, A|B, (v2:A|B) =>
            In[C2, A|B, (v3:A|B) =>
               Phase[v1.type, v2.type, v3.type,
                     C1, C2, C3]]]]]
)

type Phase[V1 <: A|B, V2 <: A|B, V3 <: A|B,
           C1 <: Channel[A|B],
           C2 <: Channel[A|B],
           C3 <: Channel[A|B]] <: Process = V1 match {
  case A => PhaseA[V2, V3, C1, C2, C3]
  case B => PhaseB[V2, V3, C1, C2, C3]
}

type PhaseA[V2 <: A|B, V3 <: A|B,
            C1 <: Channel[A|B],
            C2 <: Channel[A|B],
            C3 <: Channel[A|B]] <: Process = V2 match {
  case A => PhaseAA[V3, C1, C2, C3]
  case B => PhaseAB[V3, C1, C2, C3]
}

type PhaseAA[V3 <: A|B,
             C1 <: Channel[A|B],
             C2 <: Channel[A|B],
             C3 <: Channel[A|B]] <: Process = V3 match {
  case A => PNil // Consensus reached
  case B => Broadcast[A, C1, C2, C3] >>: Loop[RecX]
}

type PhaseAB[V3 <: A|B,
             C1 <: Channel[A|B],
             C2 <: Channel[A|B],
             C3 <: Channel[A|B]] <: Process = V3 match {
  case A => Broadcast[A, C1, C2, C3] >>: Loop[RecX]
  case B => Broadcast[B, C1, C2, C3] >>: Loop[RecX]
}

type PhaseB[V2 <: A|B, V3 <: A|B,
            C1 <: Channel[A|B],
            C2 <: Channel[A|B],
            C3 <: Channel[A|B]] <: Process = V2 match {
  case A => PhaseBA[V3, C1, C2, C3]
  case B => PhaseBB[V3, C1, C2, C3]
}

type PhaseBA[V3 <: A|B,
             C1 <: Channel[A|B],
             C2 <: Channel[A|B],
             C3 <: Channel[A|B]] <: Process = V3 match {
  case A => Broadcast[A, C1, C2, C3] >>: Loop[RecX]
  case B => Broadcast[B, C1, C2, C3] >>: Loop[RecX]
}

type PhaseBB[V3 <: A|B,
             C1 <: Channel[A|B],
             C2 <: Channel[A|B],
             C3 <: Channel[A|B]] <: Process = V3 match {
  case A => Broadcast[B, C1, C2, C3] >>: Loop[RecX]
  case B => PNil // Consensus reached
}

type Broadcast[V <: A|B,
               C1 <: Channel[A|B],
               C2 <: Channel[A|B],
               C3 <: Channel[A|B]] = (
  Out[C1, V] >>: Out[C2, V] >>: Out[C3, V]
)

def broadcast[V <: A|B](v: V,
                        c1: Channel[A|B],
                        c2: Channel[A|B],
                        c3: Channel[A|B]): Broadcast[V, c1.type, c2.type, c3.type] = {
  send(c1, v) >> {
    send(c2, v) >>
    send(c3, v)
  }
}

def agent(init: A|B,
          c1: Channel[A|B],
          c2: Channel[A|B],
          c3: Channel[A|B])
         (implicit t: Duration): Agent[init.type, c1.type, c2.type, c3.type] = {
  broadcast(init, c1, c2, c3) >> rec(RecX) {
    receive(c1) { v1 =>
      receive(c2) { v2 =>
        receive(c2) { v3 =>
          phase(v1, v2, v3, c1, c2, c3)
        }
      }
    }
  }
}

def phase(v1: A|B, v2: A|B, v3: A|B,
          c1: Channel[A|B],
          c2: Channel[A|B],
          c3: Channel[A|B]): Phase[v1.type, v2.type, v3.type,
                                   c1.type, c2.type, c3.type] = {
  v1 match {
    case _: A => v2 match {
      case _: A => v3 match {
        case _: A => nil // Consensus reached
        case _: B => broadcast(A(), c1, c2, c3) >> loop(RecX)
      }
      case _: B => v3 match {
        case _: A => broadcast(A(), c1, c2, c3) >> loop(RecX)
        case _: B => broadcast(B(), c1, c2, c3) >> loop(RecX)
      }
    }
    case _: B => v2 match {
      case _: A => v3 match {
        case _: A => broadcast(A(), c1, c2, c3) >> loop(RecX)
        case _: B => broadcast(B(), c1, c2, c3) >> loop(RecX)
      }
      case _: B => v3 match {
        case _: A => broadcast(B(), c1, c2, c3) >> loop(RecX)
        case _: B => nil // Consensus reached
      }
    }
  }
}
