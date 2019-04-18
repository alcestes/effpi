// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.plugin.benchmarks

import effpi.channel.{Channel => Chan, InChannel => IChan, OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

import effpi.verifier.{verify, pbessolve, pbes2bool_breadth, pbes2bool_depth}

/** Simplified rendering of payment service with auditor, and 8 clients. */
package object payaudit8 {
  abstract class Result
  case class Accepted() extends Result
  case class Rejected() extends Result

  /** Client protocol. */
  type Client[PayChan <: OChan[OChan[Result]], RespChan <: Chan[Result]] = Rec[RecX,
    Out[PayChan, RespChan] >>: In[RespChan, Result, (_x: Result) => Loop[RecX]]
  ]

  /** Payment service protocol. */
  type Payment[PayC <: IChan[OChan[Result]], AudC <: OChan[String]] = Rec[RecX,
    In[PayC, OChan[Result], (p: OChan[Result]) =>
      Out[p.type, Rejected] >>: Loop[RecX]
      |
      Out[AudC, String] >>: Out[p.type, Accepted] >>: Loop[RecX]
    ]
  ]

  /** Auditing service protocol. */
  type Auditor[AudChan <: IChan[String]] = Rec[RecX,
    In[AudChan, String, (_x: String) => Loop[RecX]]
  ]

  /** Payment system protocol. */
  type PaymentSys[PayChan <: Chan[OChan[Result]],
                  RespChan <: Chan[Result],
                  AudChan <: Chan[String]] = (
    Par3[ Par8[ Client[PayChan, RespChan],
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
  
  @verify(property = "no_output_use(pay)()", spec_name = "payaudit8",
          benchmark = 10, big_lts = false)
  def paymentCHK1(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???

  @verify(property = "deadlock_free()", spec_name = "payaudit8",
          benchmark = 10, big_lts = true)
  def paymentCHK2(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
  
  @verify(property = "eventual_output_use(res)()", spec_name = "payaudit8",
          benchmark = 10, big_lts = true)
  def paymentCHK3(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
  
  @verify(property = "forwarding(pay)(res)", spec_name = "payaudit8",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def paymentCHK4(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
  
  @verify(property = "reactive(pay)()", spec_name = "payaudit8",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def paymentCHK5(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???

  @verify(property = "responsive(pay)()", spec_name = "payaudit8",
          benchmark = 10, big_lts = true, solver = pbes2bool_depth)
  def paymentCHK6(pay: Chan[OChan[Result]],
                  res: Chan[Result],
                  aud: Chan[String]): PaymentSys[pay.type,
                                                 res.type,
                                                 aud.type] = ???
}
