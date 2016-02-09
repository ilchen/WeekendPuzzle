package puzzle

/**
 * Created by Andrei Ilchenko on 7-2-16.
 */
class Combi(numbers: List[Double]) {

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
  abstract class ArithmExpr(lOp: Expr, rOp: Expr, op: (Double, Double) => Double, s: String) extends Expr {
    override def evaluate(): Double = op(lOp.evaluate(), rOp.evaluate())
    override def numBinaryOps(): Int = 1 + lOp.numBinaryOps() + rOp.numBinaryOps()
    override def usedConsts(): List[Const] = lOp.usedConsts() ::: rOp.usedConsts()
    override def toString: String = "(" + lOp + s" $s " + rOp + ")"
  }
  case class Plus(lOp: Expr, rOp: Expr)   extends ArithmExpr(lOp, rOp, _ + _, "+")
  case class Minus(lOp: Expr, rOp: Expr)  extends ArithmExpr(lOp, rOp, _ - _, "-")
  case class Times(lOp: Expr, rOp: Expr)  extends ArithmExpr(lOp, rOp, _ * _, "*")
  case class Divide(lOp: Expr, rOp: Expr) extends ArithmExpr(lOp, rOp, _ / _, "/")

  val terminalExprs = numbers.map(Const)
  val arithmExprs = Plus :: Minus :: Times :: Divide :: Nil

  def generate(remOps: Int, usedNumbers: List[Const]): List[Expr] = {
    val ops = if (remOps <= 0) Nil  else  for {
      expr <- arithmExprs
      op1 <- generate(remOps - 1, usedNumbers)
      op2 <- generate(remOps - 1 - op1.numBinaryOps(), usedNumbers ::: op1.usedConsts())
    } yield expr(op1, op2)

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
