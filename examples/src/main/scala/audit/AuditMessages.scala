// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.audit

import effpi.actor.ActorRef
import java.net.URI
import java.util.UUID

case class ReqMoney[R](from: URI, amount: BigDecimal, msg: R, replyTo: ActorRef[R])

case class LogActivity(who: ActorRef[Nothing], what: String, id: Long, replyTo: ActorRef[ActivityLogged])
case class ActivityLogged(who: ActorRef[Nothing], id: Long)

sealed trait PaymentService
case class Authorize(payer: URI, amount: BigDecimal, id: UUID, replyTo: ActorRef[PaymentResult]) extends PaymentService
case class Capture(id: UUID, amount: BigDecimal, replyTo: ActorRef[PaymentResult]) extends PaymentService
case class Void(id: UUID, replyTo: ActorRef[PaymentResult]) extends PaymentService
case class Refund(id: UUID, replyTo: ActorRef[PaymentResult]) extends PaymentService

sealed trait PaymentResult
case class PaymentSuccess(id: UUID) extends PaymentResult
case class PaymentRejected(id: UUID, reason: String) extends PaymentResult
case class IdUnkwown(id: UUID) extends PaymentResult
