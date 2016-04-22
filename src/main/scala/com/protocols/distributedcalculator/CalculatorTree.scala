
import akka.actor._

/**
 * Calculator Tree actor -- Accepts computation requests with parameters of
 * type ExpressionTree, and spawns child actos to help compute result by
 * sending requests to the Calculator actor.
 */
object CalculatorTree {

  trait ParseOperation {
    def calculator: ActorRef
    def msg_id: Int
    def expr: Expr
  }


  // Sent from parent actor/node
  case class Calculate(calculator: ActorRef, msg_id: Int, expr: Expr) extends ParseOperation
  // Sent from child node
  case class Success(msg_id: Int, ans: Int) extends Calculator.SuccessfulResponse

}

class CalculatorTree extends Actor{
  import CalculatorTree._

  var receivedLeft = false
  var receivedRight = false

  var left_msg_id = -1
  var right_msg_id = -1

  var curr_msg_id = -1

  var leftVal = -1
  var rightVal = -1

  var calculator = None: Option[ActorRef]

  var parent = None: Option[ActorRef]

  var operation = ""



  override def receive = {
    // Receive calculation from child node
    case Success(msg_id, ans) =>   handleReturnedValue(msg_id, ans)
    // Receive calculation to parse and compute from parent node
    case Calculate(calc, msg_id, expr) => parent = Some(sender)
                                          calculator = Some(calc)
                                          curr_msg_id = msg_id
                                          parseInput(expr)
    // Receive value of own computation from calculator -- send back to parent
    case Calculator.Success(msg_id, result) if msg_id == curr_msg_id  =>
                                               parent.get ! Success(curr_msg_id, result)
    case Calculator.Failure(msg_id) if msg_id == curr_msg_id => parent.get ! Calculator.Failure(curr_msg_id)
    case _ => parent.get ! Calculator.Failure(-1)
  }

  def parseInput(expr: Expr) = {
    expr match {
      case Const(num) => parent.get ! Success(curr_msg_id, num)
      case BinaryOp(op, left, right) =>   left_msg_id = 0
                                          right_msg_id = 1
                                          // Set operation field
                                          operation = op

                                          val leftHandler = context.actorOf(Props[CalculatorTree], "left")
                                          val rightHandler = context.actorOf(Props[CalculatorTree], "right")
                                          leftHandler ! Calculate(calculator.get, left_msg_id, left)
                                          rightHandler ! Calculate(calculator.get, right_msg_id, right)
      case ExpressionTree(_) => parent.get ! Calculator.Failure(curr_msg_id)
    }
  }

  // When child node returns value
  def handleReturnedValue(msg_id: Int, retVal: Int) = {
    if(msg_id == left_msg_id) {
      receivedLeft = true
      leftVal = retVal
    }  else if (msg_id == right_msg_id){
      receivedRight = true
      rightVal = retVal
    } else {
       parent.get ! Calculator.Failure(curr_msg_id)
    }

    // If both children have returned their computations,
    // can perform own operation
    if(receivedRight && receivedLeft) {
        context.children foreach { child =>
          context.unwatch(child)
          context.stop(child)
        }

       operation match {
          case "*" => calculator.get ! Calculator.Mult(curr_msg_id, leftVal, rightVal)
          case "/" => calculator.get ! Calculator.Div(curr_msg_id, leftVal, rightVal)
          case "+" => calculator.get ! Calculator.Add(curr_msg_id, leftVal, rightVal)
          case "-" => calculator.get ! Calculator.Sub(curr_msg_id, leftVal, rightVal)
          case _ => parent.get ! Calculator.Failure(curr_msg_id)
        }
    }
  }

}
