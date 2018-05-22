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

    assert(e1.evaluate() == Number(17))
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

    assert(e1.evaluate() == o1)
  }

  it should "parse natural input" in {
    val e1 = 3 plus 4 by 2 into 5 minus 1
    val o1 = Subtract(Multiply(Divide(Add(Number(3), Number(4)), Number(2)), Number(5)), Number(1))

    assert(e1 eq o1)
  }

  it should "reorder natural input based on BODMAS rules" in {
    // 3 + (4 * 6 * 8) + (3 / 1)
    val e1 = 3 plus 4 into 6 into 8 plus 3 by 1
    val o1 = Add(Add(Number(3), Multiply(Multiply(Number(4), Number(6)), Number(8))), Divide(Number(3), Number(1)))

    assert(e1.reorder() eq o1)
  }

  it should "evaluate natural input" in {
    // (4 * 10) + 3 - 8 + (2 / 2) => 40 - 5 + 1 => 36
    val e1 = 4 into 10 plus 3 minus 8 plus 2 by 2
    val o1 = Number(36)

    assert(e1.reorder().evaluate() == o1)
  }

}
