package com.protocols.consensus23

import akka.actor._
import com.sun.xml.internal.ws.resources.ModelerMessages
import scala.collection.mutable.{Set => MSet}

/**
 * @author ilya
 *
 *
 *         Implementation of the 2/3-consensus protocol via Akka actors
 */

/**
 * Synchrony via rounds: send or update
 */
sealed trait RoundMessage {
  val round: Int
}
case class DoSend(round: Int, arefs: Seq[ActorRef]) extends RoundMessage
case class DoAsk(round: Int, id: ActorRef) extends RoundMessage
case class DoTell[A](round: Int, value: Option[A], id: ActorRef) extends RoundMessage



/**
 * Storing local results ofr a round @round 
 */
case class LocalRoundResults[A](round: Int, num: Int, collected: MSet[(ActorRef, A)])


class Consensus23Node[A](val myValue: A) extends Actor {

  private var expectedRound = 0
  private var storedValue: Option[A] = None

  override def receive = init

  def init: Receive = {
    // A new round has started: broadcast my own value and change the role to collect the results
    case DoSend(round, arefs: Seq[ActorRef]) if round >= expectedRound =>
      // Broadcast  messages
      for (aref <- arefs) aref ! OfferValue(round, myValue, context.self)
      // transition to the collecting phase
      val lrr: LocalRoundResults[A] = LocalRoundResults(round, arefs.size, MSet.empty)
      context.become(collectResults(lrr), discardOld = true)

    // ask for the result of the previous round  
    case DoAsk(round, id) if round + 1 == expectedRound =>
      // resond with the last message
      id ! DoTell(round, storedValue, context.self)
  }

  /**
   * Offer a value
   */
  private case class OfferValue(round: Int, value: A, id: ActorRef)


  def collectResults(res: LocalRoundResults[A]): Receive = {

    // Ignore message from other rounds
    case OfferValue(round, _, _) if round != res.round =>

    case OfferValue(_, v, id) =>
      // record the value
      res.collected += ((id, v))
      // if collected all results update and switch to the initial phase
      if (res.num == res.collected.size) {
        decideAndUpdate(res.collected.map(_._2).toSeq)
        // update round
        expectedRound = res.round + 1
        // get back to the initial state
        context.become(init)
      }
  }

  // Decide on the value according to the collected results
  private def decideAndUpdate(results: Seq[A]) = {
    val num = results.size
    if (num == 0) {
      storedValue = None
    } else {
      val freqs = results.map(e => (e, results.count(_ == e)))
      def mostFrequent(f: Int) = freqs.forall { case (_, f1) => f1 <= f }

      // Find the most frequent element
      freqs.find { case (_, f) => mostFrequent(f) } match {
        case None => storedValue = None
        case Some((e, f)) => if (3 * f >= 2 * num) {
          storedValue = Some(e)
        } else {
          storedValue = None
        }
      }
    }
  }

}