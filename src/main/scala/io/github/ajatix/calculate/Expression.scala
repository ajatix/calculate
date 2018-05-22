package io.github.ajatix.calculate

sealed trait Expression
case class Number(n: Int) extends Expression
case class Add(x: Expression, y: Expression) extends Expression
case class Subtract(x: Expression, y: Expression) extends Expression
case class Multiply(x: Expression, y: Expression) extends Expression
case class Divide(x: Expression, y: Expression) extends Expression

object Expression {

  def add(x: Number, y: Number): Number = Number(x.n + y.n)
  def subtract(x: Number, y: Number): Number = Number(x.n - y.n)
  def multiply(x: Number, y: Number): Number = Number(x.n * y.n)
  def divide(x: Number, y: Number): Number = Number(x.n / y.n)

  def evaluate(e: Expression): Number = e match {
    case n: Number => n
    case Add(x, y) => add(evaluate(x), evaluate(y))
    case Subtract(x, y) => subtract(evaluate(x), evaluate(y))
    case Multiply(x, y) => multiply(evaluate(x), evaluate(y))
    case Divide(x, y) => divide(evaluate(x), evaluate(y))
  }

}

object ExpressionDSL {
  import Expression.{evaluate => eval}

  implicit class RichExpression(e: Expression) {
    def evaluate(): Number = eval(e)
  }
}