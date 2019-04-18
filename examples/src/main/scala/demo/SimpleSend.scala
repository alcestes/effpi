// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.examples.demo

import effpi.channel.{OutChannel => OChan}
import effpi.process._
import effpi.process.dsl._

object simplesend {
  type Sender = (
    (x: String, y: OChan[String]) => Out[y.type, x.type]
  )
  
  val sender: Sender = {
    (x, y) => send(y, x)
  }
}
