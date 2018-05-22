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

}
