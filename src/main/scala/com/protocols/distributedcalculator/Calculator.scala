import akka.actor._


object Calculator {

  // Requests sent to calculator
  trait Operation {
    def msg_id: Int
    def x: Int
    def y: Int
  }

  // Responses to requests
  trait OperationResponse {
    def msg_id: Int
  }

  trait SuccessfulResponse extends OperationResponse {
    def ans: Int
  }


  case class Add(msg_id: Int, x: Int, y: Int) extends Operation
  case class Sub(msg_id: Int, x: Int, y: Int) extends Operation
  case class Mult(msg_id: Int, x: Int, y: Int) extends Operation
  case class Div(msg_id: Int, x: Int, y: Int) extends Operation

  case class Success(msg_id: Int, ans: Int) extends SuccessfulResponse
  // Failed Response has -1  as ans and id
  // (TODO: Fix pattern matching so msg_id matches)
  case class Failure(msg_id: Int) extends OperationResponse
}

class Calculator extends Actor{
  import Calculator._

  override def receive = {
    case Add(msg_id, x, y) => sender ! Success(msg_id, (x + y))
    case Sub(msg_id, x, y) => sender ! Success(msg_id, (x - y))
    case Mult(msg_id, x, y) => sender ! Success(msg_id, (x * y))
    case Div(msg_id, x, y) => if(y != 0) {
                                sender ! Success(msg_id, (x / y))
                              } else {
                                println("Error: Division by 0")
                                sender ! Failure(msg_id)
                              }
    case _ => sender ! Failure(-1)
  }
}
