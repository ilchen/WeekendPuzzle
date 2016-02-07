package puzzle

/**
 * Created by Andrei Ilchenko on 7-2-16.
 */
class Combi2(numbers: List[Double]) {

  trait  Expr {
    def evaluate(): Double
    def numBinaryOps(): Int
    def usedConsts(): List[Const]
  }
  case class Const(op: Double) extends Expr {
    override def evaluate(): Double = op
    override def numBinaryOps(): Int = 0
    override def usedConsts(): List[Const] = List(this)
    override def toString: String = op.toString
  }
  trait ArithmExpr extends Expr {
    def lOp: Expr
    def rOp: Expr
    override def numBinaryOps(): Int = 1 + lOp.numBinaryOps() + rOp.numBinaryOps()
    override def usedConsts(): List[Const] = lOp.usedConsts() ::: rOp.usedConsts()
  }
  case class Plus(lOp: Expr, rOp: Expr) extends ArithmExpr {
    override def evaluate(): Double = lOp.evaluate() + rOp.evaluate()
    override def toString: String = "(" + lOp + " + " + rOp + ")"
  }
  case class Minus(lOp: Expr, rOp: Expr) extends ArithmExpr {
    override def evaluate(): Double = lOp.evaluate() - rOp.evaluate()
    override def toString: String = "(" + lOp + " - " + rOp + ")"
  }
  case class Times(lOp: Expr, rOp: Expr) extends ArithmExpr {
    override def evaluate(): Double = lOp.evaluate() * rOp.evaluate()
    override def toString: String = "(" + lOp + " * " + rOp + ")"
  }
  case class Divide(lOp: Expr, rOp: Expr) extends ArithmExpr {
    override def evaluate(): Double = lOp.evaluate() / rOp.evaluate()
    override def toString: String = "(" + lOp + " / " + rOp + ")"
  }

  val terminalExprs = numbers.map(Const)
  val arithmExprs =  "+-*/".toList

  def generate(remOps: Int, usedNumbers: List[Const]): List[Expr] = {
    val ops = for {
      expr <- arithmExprs
      if remOps > 0
      op1 <- generate(remOps - 1, usedNumbers)
      op2 <- generate(remOps - 1 - op1.numBinaryOps(), usedNumbers ::: op1.usedConsts())
    } yield expr match {
      case '+' => Plus(op1, op2)
      case '-' => Minus(op1, op2)
      case '*' => Times(op1, op2)
      case '/' => Divide(op1, op2)
    }

    val consts = for {
      expr <- terminalExprs
      if !usedNumbers.contains(expr)
    } yield expr

    ops ++ consts
  }

  val exprs = generate(numbers.length - 1, Nil)

  def solutions(target: Double): List[Expr] = {
    for {
      expr <- exprs
      if expr.usedConsts().length == numbers.length  &&  expr.evaluate() == target
    } yield expr
  }
}
