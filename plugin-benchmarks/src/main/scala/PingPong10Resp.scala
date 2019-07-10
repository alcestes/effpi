// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.plugin.benchmarks

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.{verify, pbessolve, pbes2bool_breadth, pbes2bool_depth}

/** Ping-pong (10 pairs, responsive) verification benchmark. */
package object pingpong10resp {
  import effpi.plugin.benchmarks.pingpong6resp.{ChanA, ChanB, PingPong}
  
  type PingPongSys[C1A <: ChanA,  C1B <: ChanB,
                   C2A <: ChanA,  C2B <: ChanB,
                   C3A <: ChanA,  C3B <: ChanB,
                   C4A <: ChanA,  C4B <: ChanB,
                   C5A <: ChanA,  C5B <: ChanB,
                   C6A <: ChanA,  C6B <: ChanB,
                   C7A <: ChanA,  C7B <: ChanB,
                   C8A <: ChanA,  C8B <: ChanB,
                   C9A <: ChanA,  C9B <: ChanB,
                   C10A <: ChanA, C10B <: ChanB] = (
    Par10[ PingPong[C1A,  C1B],
           PingPong[C2A,  C2B],
           PingPong[C3A,  C3B],
           PingPong[C4A,  C4B],
           PingPong[C5A,  C5B],
           PingPong[C6A,  C6B],
           PingPong[C7A,  C7B],
           PingPong[C8A,  C8B],
           PingPong[C9A,  C9B],
           PingPong[C10A, C10B] ]
  )

  @verify(property = "no_output_use(c2a)()", spec_name = "pingpong10resp",
          benchmark = 10, big_lts = true)
  def pingpongCHK1(c1a: ChanA,  c1b: ChanB,
                   c2a: ChanA,  c2b: ChanB,
                   c3a: ChanA,  c3b: ChanB,
                   c4a: ChanA,  c4b: ChanB,
                   c5a: ChanA,  c5b: ChanB,
                   c6a: ChanA,  c6b: ChanB,
                   c7a: ChanA,  c7b: ChanB,
                   c8a: ChanA,  c8b: ChanB,
                   c9a: ChanA,  c9b: ChanB,
                   c10a: ChanA, c10b: ChanB): PingPongSys[c1a.type,  c1b.type,
                                                          c2a.type,  c2b.type,
                                                          c3a.type,  c3b.type,
                                                          c4a.type,  c4b.type,
                                                          c5a.type,  c5b.type,
                                                          c6a.type,  c6b.type,
                                                          c7a.type,  c7b.type,
                                                          c8a.type,  c8b.type,
                                                          c9a.type,  c9b.type,
                                                          c10a.type, c10b.type] = ???

  @verify(property = "deadlock_free()", spec_name = "pingpong10resp",
          benchmark = 10, big_lts = true)
  def pingpongCHK2(c1a: ChanA,  c1b: ChanB,
                   c2a: ChanA,  c2b: ChanB,
                   c3a: ChanA,  c3b: ChanB,
                   c4a: ChanA,  c4b: ChanB,
                   c5a: ChanA,  c5b: ChanB,
                   c6a: ChanA,  c6b: ChanB,
                   c7a: ChanA,  c7b: ChanB,
                   c8a: ChanA,  c8b: ChanB,
                   c9a: ChanA,  c9b: ChanB,
                   c10a: ChanA, c10b: ChanB): PingPongSys[c1a.type,  c1b.type,
                                                          c2a.type,  c2b.type,
                                                          c3a.type,  c3b.type,
                                                          c4a.type,  c4b.type,
                                                          c5a.type,  c5b.type,
                                                          c6a.type,  c6b.type,
                                                          c7a.type,  c7b.type,
                                                          c8a.type,  c8b.type,
                                                          c9a.type,  c9b.type,
                                                          c10a.type, c10b.type] = ???

  @verify(property = "eventual_output_use(c1a)()", spec_name = "pingpong10resp",
          benchmark = 10, big_lts = true)
  def pingpongCHK3(c1a: ChanA,  c1b: ChanB,
                   c2a: ChanA,  c2b: ChanB,
                   c3a: ChanA,  c3b: ChanB,
                   c4a: ChanA,  c4b: ChanB,
                   c5a: ChanA,  c5b: ChanB,
                   c6a: ChanA,  c6b: ChanB,
                   c7a: ChanA,  c7b: ChanB,
                   c8a: ChanA,  c8b: ChanB,
                   c9a: ChanA,  c9b: ChanB,
                   c10a: ChanA, c10b: ChanB): PingPongSys[c1a.type,  c1b.type,
                                                          c2a.type,  c2b.type,
                                                          c3a.type,  c3b.type,
                                                          c4a.type,  c4b.type,
                                                          c5a.type,  c5b.type,
                                                          c6a.type,  c6b.type,
                                                          c7a.type,  c7b.type,
                                                          c8a.type,  c8b.type,
                                                          c9a.type,  c9b.type,
                                                          c10a.type, c10b.type] = ???

  @verify(property = "forwarding(c1a)(c1b)", spec_name = "pingpong10resp",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def pingpongCHK4(c1a: ChanA,  c1b: ChanB,
                   c2a: ChanA,  c2b: ChanB,
                   c3a: ChanA,  c3b: ChanB,
                   c4a: ChanA,  c4b: ChanB,
                   c5a: ChanA,  c5b: ChanB,
                   c6a: ChanA,  c6b: ChanB,
                   c7a: ChanA,  c7b: ChanB,
                   c8a: ChanA,  c8b: ChanB,
                   c9a: ChanA,  c9b: ChanB,
                   c10a: ChanA, c10b: ChanB): PingPongSys[c1a.type,  c1b.type,
                                                          c2a.type,  c2b.type,
                                                          c3a.type,  c3b.type,
                                                          c4a.type,  c4b.type,
                                                          c5a.type,  c5b.type,
                                                          c6a.type,  c6b.type,
                                                          c7a.type,  c7b.type,
                                                          c8a.type,  c8b.type,
                                                          c9a.type,  c9b.type,
                                                          c10a.type, c10b.type] = ???

  @verify(property = "reactive(c5a)()", spec_name = "pingpong10resp",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def pingpongCHK5(c1a: ChanA,  c1b: ChanB,
                   c2a: ChanA,  c2b: ChanB,
                   c3a: ChanA,  c3b: ChanB,
                   c4a: ChanA,  c4b: ChanB,
                   c5a: ChanA,  c5b: ChanB,
                   c6a: ChanA,  c6b: ChanB,
                   c7a: ChanA,  c7b: ChanB,
                   c8a: ChanA,  c8b: ChanB,
                   c9a: ChanA,  c9b: ChanB,
                   c10a: ChanA, c10b: ChanB): PingPongSys[c1a.type,  c1b.type,
                                                          c2a.type,  c2b.type,
                                                          c3a.type,  c3b.type,
                                                          c4a.type,  c4b.type,
                                                          c5a.type,  c5b.type,
                                                          c6a.type,  c6b.type,
                                                          c7a.type,  c7b.type,
                                                          c8a.type,  c8b.type,
                                                          c9a.type,  c9b.type,
                                                          c10a.type, c10b.type] = ???

  @verify(property = "responsive(c2a)()", spec_name = "pingpong10resp",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def pingpongCHK6(c1a: ChanA,  c1b: ChanB,
                   c2a: ChanA,  c2b: ChanB,
                   c3a: ChanA,  c3b: ChanB,
                   c4a: ChanA,  c4b: ChanB,
                   c5a: ChanA,  c5b: ChanB,
                   c6a: ChanA,  c6b: ChanB,
                   c7a: ChanA,  c7b: ChanB,
                   c8a: ChanA,  c8b: ChanB,
                   c9a: ChanA,  c9b: ChanB,
                   c10a: ChanA, c10b: ChanB): PingPongSys[c1a.type,  c1b.type,
                                                          c2a.type,  c2b.type,
                                                          c3a.type,  c3b.type,
                                                          c4a.type,  c4b.type,
                                                          c5a.type,  c5b.type,
                                                          c6a.type,  c6b.type,
                                                          c7a.type,  c7b.type,
                                                          c8a.type,  c8b.type,
                                                          c9a.type,  c9b.type,
                                                          c10a.type, c10b.type] = ???
}
