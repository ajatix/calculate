package io.github.ajatix.calculate

sealed trait Expression {
  def eq(e: Expression): Boolean

  def plus(v: Expression): Add = Add(this, v)
  def minus(v: Expression): Subtract = Subtract(this, v)
  def into(v: Expression): Multiply = Multiply(this, v)
  def by(v: Expression): Divide = Divide(this, v)

  def unary_- : Expression = this match {
    case Number(n) => Number(-n)
    case DivideByZero => DivideByZero
    case Add(x, y) => Add(-x, -y)
    case Subtract(x, y) => Subtract(y, x)
    case Multiply(x, y) => Multiply(-x, y)
    case Divide(x, y) => Divide(-x, y)
  }
}

case class Number(n: Int) extends Expression {
  override def eq(e: Expression): Boolean = e match {
    case Number(x) => x == n
    case _ => false
  }
}

case class Add(x: Expression, y: Expression) extends Expression {
  override def eq(e: Expression): Boolean = e match {
    case Add(p, q) => ((p eq x) && (q eq y)) || ((p eq y) && (q eq x))
    case _ => false
  }
}

case class Subtract(x: Expression, y: Expression) extends Expression {
  override def eq(e: Expression): Boolean = e match {
    case Subtract(p, q) => (p eq x) && (q eq y)
    case _ => false
  }
}

case class Multiply(x: Expression, y: Expression) extends Expression {
  override def eq(e: Expression): Boolean = e match {
    case Multiply(p, q) => ((p eq x) && (q eq y)) || ((p eq y) && (q eq x))
    case _ => false
  }
}

case class Divide(x: Expression, y: Expression) extends Expression {
  override def eq(e: Expression): Boolean = e match {
    case Divide(p, q) => (p eq x) && (q eq y)
    case _ => false
  }
}

case object DivideByZero extends Expression {
  override def eq(e: Expression): Boolean = e match {
    case DivideByZero => true
    case _ => false
  }
}

object Expression {

  type Result = Either[Number, Expression]

  private def add(x: Result, y: Result): Result = (x, y) match {
    case (Left(Number(a)), Left(Number(b))) => Left(Number(a + b))
    case (Left(a), Right(b)) => Right(Add(a, b))
    case (Right(a), Left(b)) => Right(Add(a, b))
    case (Right(a), Right(b)) => Right(Add(a, b))
  }
  private def subtract(x: Result, y: Result): Result = (x, y) match {
    case (Left(Number(a)), Left(Number(b))) => Left(Number(a - b))
    case (Left(a), Right(b)) => Right(Subtract(a, b))
    case (Right(a), Left(b)) => Right(Subtract(a, b))
    case (Right(a), Right(b)) => Right(Subtract(a, b))
  }
  private def multiply(x: Result, y: Result): Result = (x, y) match {
    case (Left(Number(a)), Left(Number(b))) => Left(Number(a * b))
    case (Left(a), Right(b)) => Right(Multiply(a, b))
    case (Right(a), Left(b)) => Right(Multiply(a, b))
    case (Right(a), Right(b)) => Right(Multiply(a, b))
  }
  private def divide(x: Result, y: Result): Result = (x, y) match {
    case (Left(Number(a)), Left(Number(0))) => Right(DivideByZero)
    case (Left(Number(a)), Left(Number(b))) => Left(Number(a / b))
    case (Left(a), Right(b)) => Right(Divide(a, b))
    case (Right(a), Left(b)) => Right(Divide(a, b))
    case (Right(a), Right(b)) => Right(Divide(a, b))
  }

  def evaluateExpression(e: Expression): Result = e match {
    case n: Number => Left(n)
    case DivideByZero => Right(DivideByZero)
    case Add(x, y) => add(evaluateExpression(x), evaluateExpression(y))
    case Subtract(x, y) => subtract(evaluateExpression(x), evaluateExpression(y))
    case Multiply(x, y) => multiply(evaluateExpression(x), evaluateExpression(y))
    case Divide(x, y) => divide(evaluateExpression(x), evaluateExpression(y))
  }

  def calculateCost(e: Expression, agg: Int = 0): Int = e match {
    case n: Number => 0
    case DivideByZero => agg
    case Add(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
    case Subtract(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
    case Multiply(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
    case Divide(x, y) => agg + calculateCost(x) + calculateCost(y) + 1
  }

  def optimizeAdd(e: Add): Expression = e match {
    case Add(x, Number(0)) => x
    case Add(Number(0), y) => y
    case Add(x, y) if x eq y => Multiply(Number(2), x)
    case Add(x, y) if x eq -y => Number(0)
    case Add(x, Subtract(y, z)) if x eq z => y
    case Add(x, Subtract(y, z)) if x eq -y => -z
    case Add(Subtract(x, y), z) if y eq z => x
    case Add(Subtract(x, y), z) if x eq -z => -y
    case Add(x, y) => Add(optimizeExpression(x), optimizeExpression(y))
  }

  def optimizeSubtract(e: Subtract): Expression = e match {
    case Subtract(x, Number(0)) => x
    case Subtract(Number(0), y) => -y
    case Subtract(x, y) if x eq y => Number(0)
    case Subtract(x, y) if x eq -y => Multiply(Number(2), x)
    case Subtract(Add(x, y), z) if x eq z => y
    case Subtract(Add(x, y), z) if y eq z => x
    case Subtract(x, Add(y, z)) if x eq y => -z
    case Subtract(x, Add(y, z)) if x eq z => -y
    case Subtract(x, y) => Subtract(optimizeExpression(x), optimizeExpression(y))
  }

  def optimizeMultiply(e: Multiply): Expression = e match {
    case Multiply(x, Number(0)) => Number(0)
    case Multiply(Number(0), y) => Number(0)
    case Multiply(x, Number(1)) => x
    case Multiply(Number(1), y) => y
    case Multiply(x, Divide(y, z)) if x eq z => y
    case Multiply(Divide(x, y), z) if y eq z => x
    case Multiply(x, y) => Multiply(optimizeExpression(x), optimizeExpression(y))
  }

  def optimizeDivide(e: Divide): Expression = e match {
    case Divide(x, Number(0)) => DivideByZero
    case Divide(Number(0), y) => Number(0)
    case Divide(x, Number(1)) => x
    case Divide(x, y) if x eq y => Number(1)
    case Divide(x, Multiply(y, z)) if x eq y => Divide(Number(1), z)
    case Divide(x, Multiply(y, z)) if x eq z => Divide(Number(1), y)
    case Divide(Multiply(x, y), z) if x eq z => y
    case Divide(Multiply(x, y), z) if y eq z => x
    case Divide(x, y) => Divide(optimizeExpression(x), optimizeExpression(y))
  }

  def optimizeExpression(e: Expression): Expression = e match {
    case n: Number => n
    case DivideByZero => DivideByZero
    case e: Add => optimizeAdd(e)
    case e: Subtract => optimizeSubtract(e)
    case e: Multiply => optimizeMultiply(e)
    case e: Divide => optimizeDivide(e)
  }

  def clean(e: Expression): Expression = e match {
    case Multiply(Add(a, b), c) => Add(clean(a), Multiply(clean(b), clean(c)))
    case Multiply(Subtract(a, b), c) => Subtract(clean(a), Multiply(clean(b), clean(c)))
    case Multiply(a, b) => Multiply(clean(a), clean(b))
    case Divide(Add(a, b), c) => Add(clean(a), Divide(clean(b), clean(c)))
    case Divide(Subtract(a, b), c) => Subtract(clean(a), Divide(clean(b), clean(c)))
    case Divide(a, b) => Divide(clean(a), clean(b))
    case Add(a, b) => Add(clean(a), clean(b))
    case Subtract(a, b) => Subtract(clean(a), clean(b))
    case exp: Expression => exp
  }

  def isCleaned(e: Expression): Boolean = e match {
    case Multiply(a: Add, _) => false
    case Multiply(a: Subtract, _) => false
    case Multiply(a, b) => isCleaned(a) && isCleaned(b)
    case Divide(a: Add, _) => false
    case Divide(a: Subtract, _) => false
    case Divide(a, b) => isCleaned(a) && isCleaned(b)
    case Add(a, b) => isCleaned(a) && isCleaned(b)
    case Subtract(a, b) => isCleaned(a) && isCleaned(b)
    case _ => true
  }

  def cleanExpression(e: Expression): Expression = {
    if (!isCleaned(e)) cleanExpression(clean(e))
    else e
  }

}

object ExpressionDSL {
  import Expression._

  implicit class RichExpression(e: Expression) {
    def evaluate(): Expression = evaluateExpression(e) match {
      case Left(n) => n
      case Right(e) => e
    }
    def cost(): Int = calculateCost(e)
    def optimize(): Expression = optimizeExpression(e)
    def reorder(): Expression = cleanExpression(e)
  }

  implicit def intToNumber(n: Int): Number = Number(n)
}