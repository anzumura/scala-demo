package demo

import scala.annotation.{tailrec, unused}
import scala.language.implicitConversions

final class Rational private(n: Int, d: Int) {
  require(d != 0)
  private val g = gcd(n.abs, d.abs)
  val numer: Int = n / g
  val denom: Int = d / g

  def +(rhs: Rational): Rational = {
    if (denom == rhs.denom)
      Rational(numer + rhs.numer, denom)
    else
      Rational(numer * rhs.denom + rhs.numer * denom, denom * rhs.denom)
  }

  def +(rhs: Int): Rational = {
    Rational(numer + rhs * denom, denom)
  }

  def -(rhs: Rational): Rational = {
    this + -rhs
  }

  def -(rhs: Int): Rational = {
    Rational(numer - rhs * denom, denom)
  }

  def *(rhs: Rational): Rational = {
    Rational(numer * rhs.numer, denom * rhs.denom)
  }

  def *(rhs: Int): Rational = {
    Rational(numer * rhs, denom)
  }

  def /(rhs: Rational): Rational = {
    Rational(numer * rhs.denom, denom * rhs.numer)
  }

  def /(rhs: Int): Rational = {
    Rational(numer, denom * rhs)
  }

  @unused // to suppress IntelliJ warning
  def unary_- : Rational = {
    Rational(-numer, denom)
  }

  override def equals(rhs: Any): Boolean = rhs match {
    case x: Rational => numer == x.numer && denom == x.denom
    case _ => false
  }

  override def toString: String = {
    s"$numer/$denom"
  }

  @tailrec
  private def gcd(x: Int, y: Int): Int = {
    if (y == 0) x else gcd(y, x % y)
  }
}

object Rational {
  def apply(n: Int, d: Int = 1): Rational = new Rational(n, d)

  implicit def intToRational(x: Int): Rational = Rational(x)
}