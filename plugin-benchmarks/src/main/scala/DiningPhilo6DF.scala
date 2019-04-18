// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.plugin.benchmarks

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.{verify, pbessolve, pbes2bool_breadth, pbes2bool_depth}

/** E.W. Dijkstra's dining philosophers problem, with deadlock-free solution. */
package object diningphilo6df {
  import effpi.plugin.benchmarks.diningphilo4.{Fork, Philo}

  /** Deadlock-free dining philosophers protocol (swapped forks). */
  type Dining[C1pick <: Chan[Unit], C1drop <: Chan[Unit],
              C2pick <: Chan[Unit], C2drop <: Chan[Unit],
              C3pick <: Chan[Unit], C3drop <: Chan[Unit],
              C4pick <: Chan[Unit], C4drop <: Chan[Unit],
              C5pick <: Chan[Unit], C5drop <: Chan[Unit],
              C6pick <: Chan[Unit], C6drop <: Chan[Unit]] =
    Par12[ Philo[C1pick, C1drop, C6pick, C6drop],
           Fork[C1pick, C1drop],
           Philo[C1pick, C1drop, C2pick, C2drop],
           Fork[C2pick, C2drop],
           Philo[C2pick, C2drop, C3pick, C3drop],
           Fork[C3pick, C3drop],
           Philo[C3pick, C3drop, C4pick, C4drop],
           Fork[C4pick, C4drop],
           Philo[C4pick, C4drop, C5pick, C5drop],
           Fork[C5pick, C5drop],
           Philo[C5pick, C5drop, C6pick, C6drop],
           Fork[C6pick, C6drop] ]
  
  @verify(property = "no_output_use(p1)()", spec_name = "dining6_df",
          benchmark = 10, big_lts = false)
  def diningCHK4(p1: Chan[Unit], d1: Chan[Unit],
                 p2: Chan[Unit], d2: Chan[Unit],
                 p3: Chan[Unit], d3: Chan[Unit],
                 p4: Chan[Unit], d4: Chan[Unit],
                 p5: Chan[Unit], d5: Chan[Unit],
                 p6: Chan[Unit], d6: Chan[Unit]): Dining[p1.type, d1.type,
                                                         p2.type, d2.type,
                                                         p3.type, d3.type,
                                                         p4.type, d4.type,
                                                         p5.type, d5.type,
                                                         p6.type, d6.type] = ???

  @verify(property = "deadlock_free()", spec_name = "dining6_df",
          benchmark = 10, big_lts = true)
  def diningCHK1(p1: Chan[Unit], d1: Chan[Unit],
                 p2: Chan[Unit], d2: Chan[Unit],
                 p3: Chan[Unit], d3: Chan[Unit],
                 p4: Chan[Unit], d4: Chan[Unit],
                 p5: Chan[Unit], d5: Chan[Unit],
                 p6: Chan[Unit], d6: Chan[Unit]): Dining[p1.type, d1.type,
                                                         p2.type, d2.type,
                                                         p3.type, d3.type,
                                                         p4.type, d4.type,
                                                         p5.type, d5.type,
                                                         p6.type, d6.type] = ???
  
  @verify(property = "eventual_output_use(p1)()", spec_name = "dining6_df",
          benchmark = 10, big_lts = true)
  def diningCHK5(p1: Chan[Unit], d1: Chan[Unit],
                 p2: Chan[Unit], d2: Chan[Unit],
                 p3: Chan[Unit], d3: Chan[Unit],
                 p4: Chan[Unit], d4: Chan[Unit],
                 p5: Chan[Unit], d5: Chan[Unit],
                 p6: Chan[Unit], d6: Chan[Unit]): Dining[p1.type, d1.type,
                                                         p2.type, d2.type,
                                                         p3.type, d3.type,
                                                         p4.type, d4.type,
                                                         p5.type, d5.type,
                                                         p6.type, d6.type] = ???
  
  @verify(property = "forwarding(p1)(p2)", spec_name = "dining6_df",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def diningCHK6(p1: Chan[Unit], d1: Chan[Unit],
                 p2: Chan[Unit], d2: Chan[Unit],
                 p3: Chan[Unit], d3: Chan[Unit],
                 p4: Chan[Unit], d4: Chan[Unit],
                 p5: Chan[Unit], d5: Chan[Unit],
                 p6: Chan[Unit], d6: Chan[Unit]): Dining[p1.type, d1.type,
                                                         p2.type, d2.type,
                                                         p3.type, d3.type,
                                                         p4.type, d4.type,
                                                         p5.type, d5.type,
                                                         p6.type, d6.type] = ???
  
  @verify(property = "reactive(p1)()", spec_name = "dining6_df",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def diningCHK3(p1: Chan[Unit], d1: Chan[Unit],
                 p2: Chan[Unit], d2: Chan[Unit],
                 p3: Chan[Unit], d3: Chan[Unit],
                 p4: Chan[Unit], d4: Chan[Unit],
                 p5: Chan[Unit], d5: Chan[Unit],
                 p6: Chan[Unit], d6: Chan[Unit]): Dining[p1.type, d1.type,
                                                         p2.type, d2.type,
                                                         p3.type, d3.type,
                                                         p4.type, d4.type,
                                                         p5.type, d5.type,
                                                         p6.type, d6.type] = ???

  @verify(property = "responsive(p1)()", spec_name = "dining6_df",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def diningCHK2(p1: Chan[Unit], d1: Chan[Unit],
                 p2: Chan[Unit], d2: Chan[Unit],
                 p3: Chan[Unit], d3: Chan[Unit],
                 p4: Chan[Unit], d4: Chan[Unit],
                 p5: Chan[Unit], d5: Chan[Unit],
                 p6: Chan[Unit], d6: Chan[Unit]): Dining[p1.type, d1.type,
                                                         p2.type, d2.type,
                                                         p3.type, d3.type,
                                                         p4.type, d4.type,
                                                         p5.type, d5.type,
                                                         p6.type, d6.type] = ???
}
