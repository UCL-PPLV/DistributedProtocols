package com.protocols.consensus23

import akka.actor._
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest._


/**
 * @author ilya
 */

class TestNode[A](val round: Int, val expectedResult: Option[A]) extends Actor {
  override def receive = {
    case DoTell(r, vopt, id) if r == round =>
      println(s"At the round $round, actor $id responds with the stored value $vopt")
      assert(vopt  == expectedResult, s"the result of consensus should be $expectedResult")
  }
}

class Consensus23Tests(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with WordSpecLike with MustMatchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("Consensus23Tests"))

  override def afterAll() {
    system.shutdown()
  }

  s"all nodes" must {
    s"agree on the same value when is in majority (Some(5))" in {
      val values = List(1, 2, 5, 5, 5, 5, 5, 5, 4)

      val nodes = for (i <- values.indices)
        yield {
          val v = values(i)
          system.actorOf(Props(classOf[Consensus23Node[Int]], v), name = s"node-$i-offering-$v")
        }

      val round = 0
      val expectedResult = Some(5)
      val testNode = system.actorOf(Props(classOf[TestNode[Int]], round, expectedResult), name = s"test-node-round-$round")

      // Start the round
      for (node <- nodes) node ! DoSend(round, nodes)

      // ask the nodes for the result, so they would respond to the test node
      for (node <- nodes) node ! DoAsk(round, testNode)

    }
  }

}
