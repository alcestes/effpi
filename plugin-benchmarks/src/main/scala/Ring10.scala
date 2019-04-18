// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.plugin.benchmarks

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.{verify, pbessolve, pbes2bool_breadth, pbes2bool_depth}

/** Ring (20 elements) verification benchmark. */
package object ring10 {
  type Element[Ci <: IChan[String], Co <: OChan[String]] = Rec[RecX,
    In[Ci, String, (x: String) => Out[Co, x.type] >>: Loop[RecX]]]
  
  type ChanT = Chan[String]

  type Ring[C1 <: ChanT,  C2 <: ChanT,  C3 <: ChanT,  C4 <: ChanT,  C5 <: ChanT,
            C6 <: ChanT,  C7 <: ChanT,  C8 <: ChanT,  C9 <: ChanT,  C10 <: ChanT] = (
    Par[  Par10[ Element[C10, C1],
                 Element[C1,  C2],
                 Element[C2,  C3],
                 Element[C3,  C4],
                 Element[C4,  C5],
                 Element[C5,  C6],
                 Element[C6,  C7],
                 Element[C7,  C8],
                 Element[C8,  C9],
                 Element[C9,  C10] ],
          Out[C1, String] ]// Starts the ring loop
  )

  @verify(property = "no_output_use(c1)()", spec_name = "ring10",
          benchmark = 10, big_lts = true)
  def ringCHK1(c1: ChanT,  c2: ChanT,  c3: ChanT,  c4: ChanT,  c5: ChanT,
               c6: ChanT,  c7: ChanT,  c8: ChanT,  c9: ChanT,  c10: ChanT): Ring[c1.type,  c2.type,  c3.type,  c4.type,  c5.type,
                                                                                 c6.type,  c7.type,  c8.type,  c9.type,  c10.type] = ???

  @verify(property = "deadlock_free()", spec_name = "ring10",
          benchmark = 10, big_lts = false)
  def ringCHK2(c1: ChanT,  c2: ChanT,  c3: ChanT,  c4: ChanT,  c5: ChanT,
               c6: ChanT,  c7: ChanT,  c8: ChanT,  c9: ChanT,  c10: ChanT): Ring[c1.type,  c2.type,  c3.type,  c4.type,  c5.type,
                                                                                 c6.type,  c7.type,  c8.type,  c9.type,  c10.type] = ???

  @verify(property = "eventual_output_use(c1)()", spec_name = "ring10",
          benchmark = 10, big_lts = true)
  def ringCHK3(c1: ChanT,  c2: ChanT,  c3: ChanT,  c4: ChanT,  c5: ChanT,
               c6: ChanT,  c7: ChanT,  c8: ChanT,  c9: ChanT,  c10: ChanT): Ring[c1.type,  c2.type,  c3.type,  c4.type,  c5.type,
                                                                                 c6.type,  c7.type,  c8.type,  c9.type,  c10.type] = ???

  @verify(property = "forwarding(c1)(c2)", spec_name = "ring10",
          benchmark = 10, big_lts = true, solver = pbessolve)
  def ringCHK4(c1: ChanT,  c2: ChanT,  c3: ChanT,  c4: ChanT,  c5: ChanT,
               c6: ChanT,  c7: ChanT,  c8: ChanT,  c9: ChanT,  c10: ChanT): Ring[c1.type,  c2.type,  c3.type,  c4.type,  c5.type,
                                                                                 c6.type,  c7.type,  c8.type,  c9.type,  c10.type] = ???


  @verify(property = "reactive(c1)()", spec_name = "ring10",
          benchmark = 10, big_lts = true, solver = pbessolve)
  def ringCHK5(c1: ChanT,  c2: ChanT,  c3: ChanT,  c4: ChanT,  c5: ChanT,
               c6: ChanT,  c7: ChanT,  c8: ChanT,  c9: ChanT,  c10: ChanT): Ring[c1.type,  c2.type,  c3.type,  c4.type,  c5.type,
                                                                                 c6.type,  c7.type,  c8.type,  c9.type,  c10.type] = ???


  @verify(property = "responsive(c1)()", spec_name = "ring10",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def ringCHK6(c1: ChanT,  c2: ChanT,  c3: ChanT,  c4: ChanT,  c5: ChanT,
               c6: ChanT,  c7: ChanT,  c8: ChanT,  c9: ChanT,  c10: ChanT): Ring[c1.type,  c2.type,  c3.type,  c4.type,  c5.type,
                                                                                 c6.type,  c7.type,  c8.type,  c9.type,  c10.type] = ???

}
