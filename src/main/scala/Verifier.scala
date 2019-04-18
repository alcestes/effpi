// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.verifier

import scala.annotation.ClassfileAnnotation

/** Abstract base class for all mCRL2 solvers */
sealed abstract class Solver
case object pbessolve       extends Solver
case object pbes2bool_breadth extends Solver
case object pbes2bool_depth extends Solver

/** Annotation to trigger verification by the Effpi compiler plugin.
  *
  * @param property The property to be verified.
  * @param spec_name Name of the specification, used when benchmarking:
  *                  the results file will be called
  *                  `benchmark-<spec_name>-<property>.csv` (default: `"spec"`).
  * @param benchmark Number of benchmark runs (default: 0, i.e., no benchmark).
  *                  The plugin will issue a warning to report the location of
  *                  the benchmark results.
  * @param big_lts If true, the plugin will not try to compute the number of
  *                states of the specification, when performing benchmarks
  *                (default: false).
  *
  * @param solver Solver tool and strategy for model checking via mCRL2
  *               (default: [[pbessolve]])
  */
case class verify(property: String, spec_name: String = "spec",
                  benchmark: Int = 0,
                  big_lts: Boolean = false,
                  solver: Solver = pbessolve) extends ClassfileAnnotation
