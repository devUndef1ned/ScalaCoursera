package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions map { case (name, exprSignal) => (name, Signal(eval(exprSignal(), namedExpressions))) }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalInner(expr: Expr, refNames: List[String]): Double = expr match {
      case Literal(v) => v
      case Ref(name) if refNames.contains(name) => Double.NaN
      case Ref(name) if !refNames.contains(name) => evalInner(getReferenceExpr(name, references), name :: refNames)
      case Plus(a, b) => evalInner(a, refNames) + evalInner(b, refNames)
      case Minus(a, b) => evalInner(a, refNames) - evalInner(b, refNames)
      case Times(a, b) => evalInner(a, refNames) * evalInner(b, refNames)
      case Divide(a, b) => evalInner(a, refNames) / evalInner(b, refNames)
    }
    evalInner(expr, List())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
