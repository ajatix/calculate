package io.github.ajatix.calculate

import io.github.ajatix.calculate.ExpressionDSL._
import org.scalatest.FlatSpec

class ExpressionSpec extends FlatSpec {

  "Expression" should "support nesting of other expressions and return an Expression" in {
    val e1 = Number(3)
    val e2 = Add(Number(2), Number(4))
    val e3 = Add(Number(5), Multiply(Number(9), Number(7)))
    val e4 = Divide(Subtract(Number(3), Number(4)), Number(8))

    assert(e1.isInstanceOf[Expression])
    assert(e2.isInstanceOf[Expression])
    assert(e3.isInstanceOf[Expression])
    assert(e4.isInstanceOf[Expression])
  }

  it should "evaluate Expression" in {
    val e1 = Add(Number(5), Multiply(Number(4), Number(3)))

    assert(e1.evaluate() == Left(Number(17)))
  }

  it should "calculate cost of evaluating the Expression" in {
    val e1 = Add(Number(4), Number(8))
    val e2 = Multiply(Add(Number(6), Number(9)), Subtract(Number(1), Number(3)))

    assert(e1.cost() == 1)
    assert(e2.cost() == 3)
  }

  it should "optimize Expression" in {
    // (3 * 3) + (3 * 3) => 2 * (3 * 3)
    val e1 = Add(Multiply(Number(3), Number(3)), Multiply(Number(3), Number(3)))
    val o1 = Multiply(Number(2), Multiply(Number(3), Number(3)))

    // (3 + 0) + (3 * 1) => 3 + 3
    val e2 = Add(Add(Number(3), Number(0)), Multiply(Number(3), Number(1)))
    val o2 = Add(Number(3), Number(3))

    // (3 + (4 * 9)) - (9 * 4) => 3
    val e3 = Subtract(Add(Number(3), Multiply(Number(4), Number(9))), Multiply(Number(9), Number(4)))
    val o3 = Number(3)

    assert(e1.optimize() == o1)
    assert(e2.optimize() == o2)
    assert(e3.optimize() == o3)
  }

  it should "handle division by zero" in {
    val e1 = Add(Number(3), Divide(Number(4), Number(0)))
    val o1 = Add(Number(3), DivideByZero)

    assert(e1.evaluate() == Right(o1))
  }

}
