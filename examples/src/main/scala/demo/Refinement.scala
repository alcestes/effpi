// Refinement-ish types
package effpi.examples.demo.refinement

// import scala.compiletime.S
// import scala.compiletime.ops.int._

case class Size[A <: Seq[?]] protected (s: Seq[?]) {
  val value = s.size
}

object Size {
  def size[S <: Seq[?]](s: S): Size[S] = Size(s)
}

def size[S <: Seq[?]](s: S): Size[S] = Size.size(s)

case class Zero()

case class Succ[N <: Nat] protected (n: N) {
  val value: Int = n match {
    case _: Zero => 1
    case s: Size[?] => s.value + 1
    case m: Int => m + 1
  }
}

type Nat = Int | Size[?] | Zero | Succ[?]

sealed abstract class Compare[N1 <: Nat, N2 <: Nat]
case class LessEqual[N1 <: Nat, N2 <: Nat] protected (n1: N1, n2: N2) extends Compare[N1, N2]
case class Greater[N1 <: Nat, N2 <: Nat] protected (n1: N1, n2: N2) extends Compare[N1, N2]

object Compare {
  def compare[N1 <: Nat, N2 <: Nat](n1: N1, n2: N2): Compare[N1, N2] = {
    if (toInt(n1) <= toInt(n2)) {
      LessEqual(n1, n2)
    } else {
      Greater(n1, n2)
    }
  }

  private def toInt(n: Nat): Int = n match {
    case _: Zero => 0
    case s: Succ[?] => s.value
    case m: Int => m
    case s: Size[?] => s.value
  }
}

def compare[N1 <: Nat, N2 <: Nat](n1: N1, n2: N2): Compare[N1, N2] = {
  Compare.compare(n1, n2)
}

def s[N <: Nat](n: N): Succ[N] = Succ(n)


sealed abstract class Compare2[A, B]
case class Equal[A, B] protected (a: A, b: B) extends Compare2[A, B]
sealed abstract class NotEqual[A, B] extends Compare2[A, B]

case class Lesser[A, B] protected (a: A, b: B) extends NotEqual[A, B]
case class Greater2[A, B] protected (a: A, b: B) extends NotEqual[A, B]
case class Incomparable[A, B] protected (a: A, b: B) extends NotEqual[A, B]

object Compare2 {
  def compare[A, B](a: A, b: B): Compare2[A, B] = {
    if (a == b) {
      return Equal(a, b)
    } else {
      return Incomparable(a, b)
    }
  }
}

def compare2[A, B](a: A, b: B): Compare2[A, B] = Compare2.compare(a, b)
