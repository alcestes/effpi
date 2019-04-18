// Effpi - verified message-passing programs in Dotty
// Copyright 2019 Alceste Scalas and Elias Benussi
// Released under the MIT License: https://opensource.org/licenses/MIT
package effpi.system

import java.util.concurrent.{LinkedTransferQueue => LTQueue}

import scala.util.{Try, Success, Failure}
import scala.collection.mutable.Queue

class SchedulingQueue[E] {

  val queue = new LTQueue[E]

  def isEmpty = queue.isEmpty

  def size = queue.size()

//TODO: you might need a method to put elements in front for when you are
// recovering from the sleeping map. otherwise you might wake it up, enqueue
// it, then some process before steals their value and it forces it back to
// sleep while wasting resources and starving the sleeping processes. it might
// be better if they return with priority.

  def enqueue(elem: E): Unit = {
    //if (elem == null) {
    //  println("WARNING: enqueuing a null")
    //}
    // TODO: this return a boolean to check success, perhaps you should wrap
    // this in a try
    queue.add(elem)
  }

  def dequeue(): Option[E] = {
    try { Some(queue.take()) } catch {
        case e: InterruptedException =>
          None
      }
  }

}
