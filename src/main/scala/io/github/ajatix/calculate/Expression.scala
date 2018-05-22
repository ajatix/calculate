package io.github.ajatix.calculate

sealed trait Expression
case class Number(n: Int) extends Expression
case class Add(x: Expression, y: Expression) extends Expression
case class Subtract(x: Expression, y: Expression) extends Expression
case class Multiply(x: Expression, y: Expression) extends Expression
case class Divide(x: Expression, y: Expression) extends Expression

object Expression {

  private def add(x: Number, y: Number): Number = Number(x.n + y.n)
  private def subtract(x: Number, y: Number): Number = Number(x.n - y.n)
  private def multiply(x: Number, y: Number): Number = Number(x.n * y.n)
  private def divide(x: Number, y: Number): Number = Number(x.n / y.n)

  def evaluate(e: Expression): Number = e match {
    case n: Number => n
    case Add(x, y) => add(evaluate(x), evaluate(y))
    case Subtract(x, y) => subtract(evaluate(x), evaluate(y))
    case Multiply(x, y) => multiply(evaluate(x), evaluate(y))
    case Divide(x, y) => divide(evaluate(x), evaluate(y))
  }

  def calculateCost(e: Expression, agg: Int = 0): Int = e match {
    case n: Number => 0
    case Add(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
    case Subtract(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
    case Multiply(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
    case Divide(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
  }

}

object ExpressionDSL {
  import Expression.{evaluate => eval, _}

  implicit class RichExpression(e: Expression) {
    def evaluate(): Number = eval(e)
    def cost(): Int = calculateCost(e)
  }
}