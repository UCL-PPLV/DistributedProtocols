import scala.util.control.Breaks._
sealed abstract class Expr

case class Const(val num: Int) extends Expr
case class BinaryOp(val op: String, val left: Expr, val right: Expr) extends Expr

case class ExpressionTree(val inputExpr: String) extends Expr {

  var root : Expr = createSubtree(inputExpr)


  def createSubtree(expression: String): Expr = {
    try {
      var cleaned =  expression.stripPrefix("(").stripSuffix(")").trim
      val parsedNum = cleaned.toInt
      val leaf  =  new Const(parsedNum)
      leaf;
    } catch {
      case ne: NumberFormatException =>   var expr  = expression.stripPrefix("(").trim
                                          val operation = expr.charAt(0).toString
                                          expr = expr.stripPrefix(operation).trim
                                          var countParens : Int = 0
                                          var leftIndex : Int = -1
                                          var rightIndex : Int = -1
                                          var i = 0
                                          for( i <- 0 until expr.length) {
                                            var char = expr.charAt(i).toString
                                            if(char.equals("(")) {
                                              countParens+= 1
                                              if ((countParens == 1) && (i > 0)) {
                                                // case: (+ a (+ b c))
                                                leftIndex = i
                                              }

                                            } else if(char.equals(")")) {
                                              countParens -= 1
                                              if(countParens == 0) {
                                                // At end of subtree expr --
                                                // set left or right index
                                                if(leftIndex == -1) {
                                                  leftIndex = i
                                                } else {
                                                  rightIndex = i
                                                }
                                              } else if(countParens < 0) {
                                                if(rightIndex == -1) {
                                                  // Case: (+ a b)
                                                  // or (+ (+ a b) c)
                                                  rightIndex = i
                                                }
                                                if(leftIndex == -1) {
                                                  // Case: (+ a b)
                                                  // pretty hacky here
                                                  leftIndex = expr.indexOf(" ")
                                                }
                                              }
                                            }
                                          }
                                          if(leftIndex == -1 || rightIndex == -1) {
                                            throw new IllegalArgumentException("Invalid Input")
                                            Const(-1)
                                          } else {
                                            var root = new BinaryOp(operation, createSubtree(expr.substring(0, leftIndex)),
                                                                               createSubtree(expr.substring(leftIndex, rightIndex+1)))
                                            root;
                                          }

      case e: Exception => Const(-1)
    }


  }
}
