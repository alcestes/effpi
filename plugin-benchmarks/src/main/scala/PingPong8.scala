// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.plugin.benchmarks

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.{verify, pbessolve, pbes2bool_breadth, pbes2bool_depth}

/** Ping-pong (8 pairs, non-responsive) verification benchmark. */
package object pingpong8 {
  import effpi.plugin.benchmarks.pingpong6.{ChanT, PingPong}
  
  type PingPongSys[C1A <: ChanT, C1B <: ChanT,
                   C2A <: ChanT, C2B <: ChanT,
                   C3A <: ChanT, C3B <: ChanT,
                   C4A <: ChanT, C4B <: ChanT,
                   C5A <: ChanT, C5B <: ChanT,
                   C6A <: ChanT, C6B <: ChanT,
                   C7A <: ChanT, C7B <: ChanT,
                   C8A <: ChanT, C8B <: ChanT] = (
    Par8[ PingPong[C1A, C1B],
          PingPong[C2A, C2B],
          PingPong[C3A, C3B],
          PingPong[C4A, C4B],
          PingPong[C5A, C5B],
          PingPong[C6A, C6B],
          PingPong[C7A, C7B],
          PingPong[C8A, C8B] ]
  )

  @verify(property = "no_output_use(c2a)()", spec_name = "pingpong8",
          benchmark = 10, big_lts = true)
  def pingpongCHK1(c1a: ChanT, c1b: ChanT,
                   c2a: ChanT, c2b: ChanT,
                   c3a: ChanT, c3b: ChanT,
                   c4a: ChanT, c4b: ChanT,
                   c5a: ChanT, c5b: ChanT,
                   c6a: ChanT, c6b: ChanT,
                   c7a: ChanT, c7b: ChanT,
                   c8a: ChanT, c8b: ChanT): PingPongSys[c1a.type, c1b.type,
                                                        c2a.type, c2b.type,
                                                        c3a.type, c3b.type,
                                                        c4a.type, c4b.type,
                                                        c5a.type, c5b.type,
                                                        c6a.type, c6b.type,
                                                        c7a.type, c7b.type,
                                                        c8a.type, c8b.type] = ???

  @verify(property = "deadlock_free()", spec_name = "pingpong8",
          benchmark = 10, big_lts = false)
  def pingpongCHK2(c1a: ChanT, c1b: ChanT,
                   c2a: ChanT, c2b: ChanT,
                   c3a: ChanT, c3b: ChanT,
                   c4a: ChanT, c4b: ChanT,
                   c5a: ChanT, c5b: ChanT,
                   c6a: ChanT, c6b: ChanT,
                   c7a: ChanT, c7b: ChanT,
                   c8a: ChanT, c8b: ChanT): PingPongSys[c1a.type, c1b.type,
                                                        c2a.type, c2b.type,
                                                        c3a.type, c3b.type,
                                                        c4a.type, c4b.type,
                                                        c5a.type, c5b.type,
                                                        c6a.type, c6b.type,
                                                        c7a.type, c7b.type,
                                                        c8a.type, c8b.type] = ???

  @verify(property = "eventual_output_use(c1a)()", spec_name = "pingpong8",
          benchmark = 10, big_lts = true)
  def pingpongCHK3(c1a: ChanT, c1b: ChanT,
                   c2a: ChanT, c2b: ChanT,
                   c3a: ChanT, c3b: ChanT,
                   c4a: ChanT, c4b: ChanT,
                   c5a: ChanT, c5b: ChanT,
                   c6a: ChanT, c6b: ChanT,
                   c7a: ChanT, c7b: ChanT,
                   c8a: ChanT, c8b: ChanT): PingPongSys[c1a.type, c1b.type,
                                                        c2a.type, c2b.type,
                                                        c3a.type, c3b.type,
                                                        c4a.type, c4b.type,
                                                        c5a.type, c5b.type,
                                                        c6a.type, c6b.type,
                                                        c7a.type, c7b.type,
                                                        c8a.type, c8b.type] = ???

  @verify(property = "forwarding(c1a)(c1b)", spec_name = "pingpong8",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def pingpongCHK4(c1a: ChanT, c1b: ChanT,
                   c2a: ChanT, c2b: ChanT,
                   c3a: ChanT, c3b: ChanT,
                   c4a: ChanT, c4b: ChanT,
                   c5a: ChanT, c5b: ChanT,
                   c6a: ChanT, c6b: ChanT,
                   c7a: ChanT, c7b: ChanT,
                   c8a: ChanT, c8b: ChanT): PingPongSys[c1a.type, c1b.type,
                                                        c2a.type, c2b.type,
                                                        c3a.type, c3b.type,
                                                        c4a.type, c4b.type,
                                                        c5a.type, c5b.type,
                                                        c6a.type, c6b.type,
                                                        c7a.type, c7b.type,
                                                        c8a.type, c8b.type] = ???

  @verify(property = "reactive(c5a)()", spec_name = "pingpong8",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def pingpongCHK5(c1a: ChanT, c1b: ChanT,
                   c2a: ChanT, c2b: ChanT,
                   c3a: ChanT, c3b: ChanT,
                   c4a: ChanT, c4b: ChanT,
                   c5a: ChanT, c5b: ChanT,
                   c6a: ChanT, c6b: ChanT,
                   c7a: ChanT, c7b: ChanT,
                   c8a: ChanT, c8b: ChanT): PingPongSys[c1a.type, c1b.type,
                                                        c2a.type, c2b.type,
                                                        c3a.type, c3b.type,
                                                        c4a.type, c4b.type,
                                                        c5a.type, c5b.type,
                                                        c6a.type, c6b.type,
                                                        c7a.type, c7b.type,
                                                        c8a.type, c8b.type] = ???

  @verify(property = "responsive(c2a)()", spec_name = "pingpong8",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def pingpongCHK6(c1a: ChanT, c1b: ChanT,
                   c2a: ChanT, c2b: ChanT,
                   c3a: ChanT, c3b: ChanT,
                   c4a: ChanT, c4b: ChanT,
                   c5a: ChanT, c5b: ChanT,
                   c6a: ChanT, c6b: ChanT,
                   c7a: ChanT, c7b: ChanT,
                   c8a: ChanT, c8b: ChanT): PingPongSys[c1a.type, c1b.type,
                                                        c2a.type, c2b.type,
                                                        c3a.type, c3b.type,
                                                        c4a.type, c4b.type,
                                                        c5a.type, c5b.type,
                                                        c6a.type, c6b.type,
                                                        c7a.type, c7b.type,
                                                        c8a.type, c8b.type] = ???
}
