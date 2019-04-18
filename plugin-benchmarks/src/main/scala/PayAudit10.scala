// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.plugin.benchmarks

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.{verify, pbessolve, pbes2bool_breadth, pbes2bool_depth}

/** Simplified rendering of payment service with auditor, and 10 clients. */
package object payaudit10 {
  import effpi.plugin.benchmarks.payaudit8.{Result, Client, Payment, Auditor}

  /** Payment system protocol. */
  type PaymentSys[PayChan <: Chan[OChan[Result]],
                  RespChan <: Chan[Result],
                  AudChan <: Chan[String]] = (
    Par3[ Par10[ Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan],
                 Client[PayChan, RespChan] ],
         Payment[PayChan, AudChan],
         Auditor[AudChan] ]
    )
  
  @verify(property = "no_output_use(pay)()", spec_name = "payaudit10",
          benchmark = 10, big_lts = false)
  def paymentCHK1(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???

  @verify(property = "deadlock_free()", spec_name = "payaudit10",
          benchmark = 10, big_lts = true)
  def paymentCHK2(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
  
  @verify(property = "eventual_output_use(res)()", spec_name = "payaudit10",
          benchmark = 10, big_lts = true)
  def paymentCHK3(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
  
  @verify(property = "forwarding(pay)(res)", spec_name = "payaudit10",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def paymentCHK4(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
  
  @verify(property = "reactive(pay)()", spec_name = "payaudit10",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def paymentCHK5(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???

  @verify(property = "responsive(pay)()", spec_name = "payaudit10",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def paymentCHK6(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
}
