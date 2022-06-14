package es.tmoor.scanvas

import Fraction.FractionComparisonOps.mkNumericOps
import Fraction.FractionComparisonOps.mkOrderingOps

object Fraction:
  private def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a%b)
  def apply(n: Long, d: Long): Fraction =
    val g = gcd(n,d)
    val a_ = n / g
    val b_ = d / g
    val (a,b) = if (b_ < 0) (-a_, -b_) else (a_, b_)
    if (a / Int.MaxValue != 0 || b / Int.MaxValue != 0)
      Fraction(a / Short.MaxValue, b / Short.MaxValue)
    else new Fraction(a, b)

  def unapply(f: Fraction): Option[(Int, Int)] =
    if (f.den != 0) Some(f.num.toInt, f.den.toInt) else None

  implicit object FractionComparisonOps extends Fractional[Fraction]:
    def compare(x: Fraction, y: Fraction): Int = x.toDouble compare y.toDouble
    def fromInt(x: Int): Fraction = x¦1
    def div(x: Fraction, y: Fraction): Fraction =
      Fraction(x.num * y.den, x.den * y.num)
    def minus(x: Fraction, y: Fraction): Fraction =
      Fraction(x.num * y.den - y.num * x.den, x.den * y.den)
    def negate(x: Fraction): Fraction = Fraction(-x.num, x.den)
    def parseString(str: String): Option[Fraction] = None // Cba haha
    def plus(x: Fraction, y: Fraction): Fraction =
      Fraction(x.num * y.den + y.num * x.den, x.den * y.den)
    def times(x: Fraction, y: Fraction): Fraction =
      Fraction(x.num * y.num, x.den * y.den)
    def toInt(x: Fraction): Int = (x.num / x.den).toInt
    def toLong(x: Fraction): Long = (x.num / x.den).toLong
    def toFloat(x: Fraction): Float = toDouble(x).toFloat
    def toDouble(x: Fraction): Double = (x.num.toDouble / x.den.toDouble)


extension (i: Int) def ¦(that: Int) = Fraction(that, i)

class Fraction private (val num: Long, val den: Long):
  override def toString = s"$num/$den"
  override def equals(x: Any): Boolean = x match {
    case Fraction(`num`, `den`) => true
    case _ => false
  }
  def in(lo: Fraction, hi: Fraction): Boolean = this > lo && this < hi
  def *(that: Int) =
    Fraction(this.num * that, this.den)
  def /(that: Int) =
    Fraction(this.num, this.den * that)
  def +(that: Int) =
    Fraction(this.num + that * this.den, this.den)
  def -(that: Int) =
    Fraction(this.num - that * this.den, this.den)
