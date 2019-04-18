// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.verifier.mcrl2

/** An mCRL2 process, constructed by encoding a {@code Spec} instance.
  * 
  * A process consists of a {@code current} part (being built by the
  * encoding), and a series of process definitions ({@code defs}). The
  * two will be later merged in a single string, and then in a single
  * {@code .mcrl2} file
  */
protected[mcrl2]
case class Proc(current: List[String] = List(),
                defs: List[String] = List()) {
  /** Turn the {@code current} part into a sequential mCRL2 process */
  def sequential: Proc = Proc(
    // We need to ignore empty strings
    List(current.mkString(" . ")).filter(!_.isEmpty),
    defs
  )

  /** Compose two {@code Proc} instances, turning them into a choice
    * between sequential processes, and combining their definitions.
    */
  def +(that: Proc) = Proc(
    List({
           val cur = (this.sequential.current ++
                        that.sequential.current).mkString(" + ")
           // Add parentheses for correct precedence
           if (!cur.isEmpty) "(" ++ cur ++ ")" else cur
         }),
    defs ++ that.defs
  )

  /** Compose two specs, turning them into a parallel composition
    * of sequential mCRL2 processes, and combining their definitions.
    */
  def ||(that: Proc) = Proc(
    List(
      (this.sequential.current ++ that.sequential.current).mkString(" || ")
    ),
    defs ++ that.defs
  )

  /** Add a tau prefix to the current process */
  def tauPrefix: Proc = Proc("tau" +: current, defs)

  /** Turn the {@code current} part into a sequential mCRL2 process
    * definition, and make it part of the {@code defs}.
    */
  def toDef(proc: String): Proc = {
    val procBody = {
      if (current.length == 0) "delta"
      else current.mkString(" . ")
    }
    Proc(
      List(),
      List(s"""proc ${proc} = ${procBody};""") ++ defs
    )
  }

  /** Represent the process specification as a string. */
  def show: String = {
    current.mkString(" %%% CURRENT\n") ++ "\n" ++ defs.mkString("\n\n")
  }

  /** Render the specification. This requires {@code current} to be empty! */
  def render: String = {
    assert(current.isEmpty)
    defs.mkString("\n\n")
  }
}
