// Effpi - verified message-passing programs in Dotty
// Copyright 2020 Alceste Scalas
// Released under the MIT License: https://opensource.org/licenses/MIT

// This is the two-buyer protocol example from session types literature,
// implemented using match types
package effpi.examples.demo.twobuyuer

import scala.concurrent.duration.Duration

import effpi.process._
import effpi.channel.{Channel, InChannel, OutChannel}
import java.time.LocalDate

package types {
  import effpi.process.dsl._

  // Message classes
  case class Quote(amount: Int)
  case class NotAvailable()

  case class Contrib(amount: Int)
  case class Cancel()
  case class OK()
  case class Negotiate()

  case class Buy(address: String)
  case class Confirm(delivery: LocalDate)
  
  // Seller protocol
  type Seller[CTitle <: InChannel[String],
              CQuote1 <: OutChannel[Quote | NotAvailable],
              CQuote2 <: OutChannel[Quote | NotAvailable],
              CResp <: InChannel[Buy | Cancel],
              CConf <: OutChannel[Confirm]] =
    In[CTitle, String, String =>
        (Out[CQuote1, NotAvailable] >>: Out[CQuote2, NotAvailable])
        |
        TrySell[CQuote1, CQuote2, CResp, CConf]
      ]
  
  type TrySell[CQuote1 <: OutChannel[Quote | NotAvailable],
               CQuote2 <: OutChannel[Quote | NotAvailable],
               CResp <: InChannel[Buy | Cancel],
               CConf <: OutChannel[Confirm]] =
    (Out[CQuote1, Quote] >>: Out[CQuote2, Quote] >>:
      In[CResp, Buy|Cancel, (x: Buy|Cancel) => SellerMatch[x.type, CConf]])
  
  type SellerMatch[X <: Buy|Cancel, CConf <: OutChannel[Confirm]] <: Process = X match {
    case Buy => Out[CConf, Confirm]
    case Cancel => PNil
  }

  // Alice's protocol
  type Alice[CTitle <: OutChannel[String],
             CQuote <: InChannel[Quote | NotAvailable],
             CBobContrib <: OutChannel[Contrib|Cancel],
             CBobResp <: InChannel[OK|Cancel|Negotiate],
             CResp <: OutChannel[Buy|Cancel],
             CConf <: InChannel[Confirm]] =
    Out[CTitle, String] >>:
    In[CQuote, Quote|NotAvailable, (x: Quote|NotAvailable) =>
      QuoteMatch[x.type, CBobContrib, CBobResp, CResp, CConf]
    ]

  type QuoteMatch[X <: Quote|NotAvailable,
                  CBobContrib <: OutChannel[Contrib|Cancel],
                  CBobResp <: InChannel[OK|Cancel|Negotiate],
                  CResp <: OutChannel[Buy | Cancel],
                  CConf <: InChannel[Confirm]] <: Process = X match {
    case NotAvailable => Out[CBobContrib, Cancel]
    case Quote => Rec[RecX,
      (Out[CBobContrib, Cancel] >>: Out[CResp, Cancel])
      |
      (Out[CBobContrib, Contrib] >>:
       In[CBobResp, OK|Cancel|Negotiate, (x: OK|Cancel|Negotiate) =>
          BobRespMatch[x.type, CResp, CConf]
       ])
    ]
  }
  
  type BobRespMatch[X <: OK|Cancel|Negotiate,
                    CResp <: OutChannel[Buy|Cancel],
                    CConf <: InChannel[Confirm]] <: Process = X match {
    case Cancel => Out[CResp, Cancel]
    case Negotiate => Loop[RecX]
    case OK => Out[CResp, Buy] >>: In[CConf, Confirm, Confirm => PNil]
  }
}

package implementation {
  import types._
  implicit val timeout: Duration = Duration("30 seconds")
  import effpi.process.dsl._

  // Seller process
  def seller(cTitle: InChannel[String],
             cQuote1: OutChannel[Quote | NotAvailable],
             cQuote2: OutChannel[Quote | NotAvailable],
             cResp: InChannel[Buy | Cancel],
             cConf: OutChannel[Confirm]): Seller[cTitle.type, cQuote1.type, cQuote2.type, cResp.type, cConf.type] = {
    receive(cTitle) {
      case "Book A" => sell(50, cQuote1, cQuote2, cResp, cConf)
      case "Book B" => sell(99, cQuote1, cQuote2, cResp, cConf)
      case _ => send(cQuote1, NotAvailable()) >> send(cQuote2, NotAvailable())
    }
  }

  def sell(amount: Int, cQuote1: OutChannel[Quote|NotAvailable], cQuote2: OutChannel[Quote|NotAvailable],
           cResp: InChannel[Buy | Cancel], cConf: OutChannel[Confirm]): TrySell[cQuote1.type, cQuote2.type, cResp.type, cConf.type] = {
    send(cQuote1, Quote(amount)) >> {
      send(cQuote2, Quote(amount)) >>
      receive(cResp) { (res: Buy|Cancel) => res match {
        case b: Buy => {
          println("Shipping to: ${b.address)")
          send(cConf, Confirm(LocalDate.now().plusWeeks(1)))
        }
        case _: Cancel => nil
      } }
    }
  }

  // Alice's process
  def alice(cTitle: OutChannel[String],
            cQuote: InChannel[Quote | NotAvailable],
            cBobContrib: OutChannel[Contrib|Cancel],
            cBobResp: InChannel[OK|Cancel|Negotiate],
            cResp: OutChannel[Buy | Cancel],
            cConf: InChannel[Confirm]): Alice[cTitle.type, cQuote.type, cBobContrib.type, cBobResp.type, cResp.type, cConf.type] = {
    send(cTitle, "Book A") >> {
      receive(cQuote) { (q: Quote|NotAvailable) =>
        handleQuote(q, cBobContrib, cBobResp, cResp, cConf)
      }
    } 
  }

  def handleQuote(q: Quote|NotAvailable, cBobContrib: OutChannel[Contrib|Cancel],
                  cBobResp: InChannel[OK|Cancel|Negotiate],
                  cResp: OutChannel[Buy | Cancel],
                  cConf: InChannel[Confirm]): QuoteMatch[q.type, cBobContrib.type,
                                                         cBobResp.type, cResp.type,
                                                         cConf.type] = q match {
        case _: NotAvailable => send(cBobContrib, Cancel())
        case q: Quote => rec(RecX) {
          var iterations = 1
          if (q.amount > 1000) {
            send(cBobContrib, Cancel()) >> send(cResp, Cancel())
          } else {
            val contrib = q.amount / (iterations + 1)
            send(cBobContrib, Contrib(contrib)) >>
            receive(cBobResp) { (res: OK|Cancel|Negotiate) =>
              bobRespMatch(res, cResp, cConf)
            }
          }
        }
      }
  
  def bobRespMatch(res: OK|Cancel|Negotiate,
                   cResp: OutChannel[Buy | Cancel],
                   cConf: InChannel[Confirm]): BobRespMatch[res.type, cResp.type, cConf.type] = {
       res match {
          case _: Cancel => send(cResp, Cancel())
          case _: Negotiate => /* iterations += 1; */ loop(RecX)
          case _: OK => {
            send(cResp, Buy("Street 1, Copenhagen")) >>
            receive(cConf) { (conf: Confirm) =>
              println("Purchase confirmed; delivery on ${conf.delivery}")
              nil
            }
          }
        }
  }
}

// To run this example, try:
// sbt "examples/runMain effpi.examples.demo.twobuyer.Main"
object Main {
  import types._
  import implementation._

  def main(): Unit = main(Array())

  def main(args: Array[String]) = {
    implicit val ps = effpi.system.ProcessSystemRunnerImproved()

    Thread.sleep(5000) // Wait 5 seconds
    ps.kill()
  }
}
