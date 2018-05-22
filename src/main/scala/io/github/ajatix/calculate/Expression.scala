package io.github.ajatix.calculate

sealed trait Expression {
  def evaluate(): Expression = ???
}
case class Number(x: Int) extends Expression
case class Add(x: Expression, y: Expression) extends Expression
case class Subtract(x: Expression, y: Expression) extends Expression
case class Multiply(x: Expression, y: Expression) extends Expression
case class Divide(x: Expression, y: Expression) extends Expression