// Refinement-ish types
package effpi.examples.demo.refinement

// import scala.compiletime.S
// import scala.compiletime.ops.int._

sealed abstract class BaseNat {
  val value: Int
}

case class Size[A <: Seq[?]] protected (s: Seq[?]) extends BaseNat {
  val value = s.size
}

def size[S <: Seq[?]](s: S): Size[S] = Size(s)

case class Zero() extends BaseNat {
  override val value = 0
}

sealed abstract class UnaryOp[N <: Nat] extends BaseNat

case class Succ[N <: Nat] protected (n: N) extends UnaryOp[N] {
  val value: Int = n match {
    case b: BaseNat => b.value + 1
    case m: Int => m + 1
  }
}

sealed abstract class BinaryOp[N1 <: Nat, N2 <: Nat] extends BaseNat

case class Div[N1 <: Nat, N2 <: Nat] protected (n1: N1, n2: N2) extends BinaryOp[N1, N2] {
  override val value = toInt(n1) / toInt(n2)
}

def div[N1 <: Nat, N2 <: Nat](n1: N1, n2: N2) = Div(n1, n2)

type Nat = Int | BaseNat

sealed abstract class Compare[N1 <: Nat, N2 <: Nat]
case class LessEqual[N1 <: Nat, N2 <: Nat] protected (n1: N1, n2: N2) extends Compare[N1, N2]
case class Greater[N1 <: Nat, N2 <: Nat] protected (n1: N1, n2: N2) extends Compare[N1, N2]

def compare[N1 <: Nat, N2 <: Nat](n1: N1, n2: N2): Compare[N1, N2] = {
  if (toInt(n1) <= toInt(n2)) {
    LessEqual(n1, n2)
  } else {
    Greater(n1, n2)
  }
}

def succ[N <: Nat](n: N): Succ[N] = Succ(n)

sealed abstract class Compare2[A, B]
case class Equal[A, B] protected (a: A, b: B) extends Compare2[A, B]
sealed abstract class NotEqual[A, B] extends Compare2[A, B]

case class Lesser[A, B] protected (a: A, b: B) extends NotEqual[A, B]
case class Greater2[A, B] protected (a: A, b: B) extends NotEqual[A, B]
case class Incomparable[A, B] protected (a: A, b: B) extends NotEqual[A, B]

def compare2[A, B](a: A, b: B): Compare2[A, B] = {
  if (a == b) {
    return Equal(a, b)
  } else {
    return Incomparable(a, b)
  }
}

private def toInt(n: Nat): Int = n match {
  case b: BaseNat => b.value
  case m: Int => m
}
