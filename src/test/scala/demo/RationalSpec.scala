package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RationalSpec extends AnyFlatSpec with Matchers {
  behavior of "Rational"

  it should "construct from numerator and denominator" in {
    val r = Rational(2, 5)
    r.numer shouldBe 2
    r.denom shouldBe 5
  }

  it should "default denominator to 1 if not provided" in {
    val r = Rational(6)
    r.numer shouldBe 6
    r.denom shouldBe 1
  }

  it should "support equals" in {
    Rational(2, 3) shouldBe Rational(2, 3)
    Rational(1, 3) should not equal Rational(2, 3)
    Rational(1, 3) should not equal 7
  }

  it should "reduce to normalized form" in {
    Rational(21, 28) shouldBe Rational(3, 4)
  }

  it should "convert to string" in {
    Rational(3, 4).toString shouldBe "3/4"
    Rational(9).toString shouldBe "9/1"
  }

  it should "support addition" in {
    val a = Rational(4, 5)
    val b = Rational(6, 5)
    a + b shouldBe Rational(2)
  }

  it should "support addition with different denominators" in {
    val a = Rational(3, 8)
    val b = Rational(1, 2)
    a + b shouldBe Rational(7, 8)
  }

  it should "support addition with integers" in {
    Rational(2, 7) + 2 shouldBe Rational(16, 7)
  }

  it should "support subtraction" in {
    val a = Rational(4, 5)
    val b = Rational(6, 5)
    a - b shouldBe Rational(-2, 5)
  }

  it should "support subtraction with different denominators" in {
    val a = Rational(3, 8)
    val b = Rational(1, 2)
    a - b shouldBe Rational(-1, 8)
  }

  it should "support subtraction with integers" in {
    Rational(2, 7) - 2 shouldBe Rational(-12, 7)
  }

  it should "support multiplication" in {
    val a = Rational(2, 3)
    val b = Rational(1, 2)
    a * b shouldBe Rational(1, 3)
  }

  it should "support multiplication with integers" in {
    Rational(2, 3) * 3 shouldBe Rational(2)
  }

  it should "support division" in {
    val a = Rational(2, 3)
    val b = Rational(1, 2)
    a / b shouldBe Rational(4, 3)
  }

  it should "support division with integers" in {
    Rational(2, 3) / 3 shouldBe Rational(2, 9)
  }

  it should "support unary minus" in {
    -Rational(2, 9) shouldBe Rational(-2, 9)
    -Rational(-2, 7) shouldBe Rational(2, 7)
  }

  it should "support implicit conversion from integer" in {
    3 + Rational(4, 5) shouldBe Rational(19, 5)
  }
}