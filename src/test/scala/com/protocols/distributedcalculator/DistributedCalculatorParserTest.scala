import akka.actor._
import akka.testkit._
import org.scalatest._
import scala.concurrent.duration._

class DistributedCalculatorParserTest(_system: ActorSystem) extends TestKit(_system) with ImplicitSender with WordSpecLike with MustMatchers with BeforeAndAfterAll {

  // Necessary for tests to be run (?)
  def this() = this(ActorSystem("DistributedCalculatorParserTest"))


    s"Result" must {
      s"be successful addition" in {
        setupAndTestOperation(101, "(+ 2 3)", 5)
      }

      s"be successful subtraction" in {
        setupAndTestOperation(202, "(- 10 5)", 5)
      }

      s"be successful multiplication" in {
        setupAndTestOperation(303, "(* 4 3)", 12)
      }

      s"be successful division" in {
        setupAndTestOperation(404, "(/ 6 2)", 3)
      }

      s"be successful mixed operations" in {
        setupAndTestOperation(505, "(* (+ 2 3) (- 6 3))", 15)
      }

      s"be more successful mixed operations" in {
        setupAndTestOperation(606, "(* (+ 3 3) (+ 4 6))", 60)
      }

      s"be even more successful mixed operations" in {
        setupAndTestOperation(707, "(* (+ 3 (+ 1 2)) (+ (- 8 4) (/ 12 2)))", 60)
      }

      // s"be unsuccessful division" in {
      //   setupAndTestOperation(606, "(/ 3 0)", -1)
      // }

    }


      def setupAndTestOperation[A](msg_id: Int, expr: String,
                               expectedResult: Int): Unit = {

          val system = ActorSystem()
          // create the calculator
          val calc = system.actorOf(Props[Calculator], "calculator")
          val calculatorTreeRoot = system.actorOf(Props[CalculatorTree], "calculatorTreeRoot")
          val expressionTree = new ExpressionTree(expr)

          calculatorTreeRoot ! CalculatorTree.Calculate(calc, msg_id, expressionTree.root)
          val res = receiveOne(30 seconds)

          res match {
            case CalculatorTree.Success(curr_msg_id, parsedNum) =>
                assert(parsedNum == expectedResult, s"the result of calculation should be $expectedResult")
            case Calculator.Failure(msg_id) => println("Test Failure"); assert(1 == 0, "Failure succeeded")
          }
          println()

      }



}
