package es.tmoor.scanvas

import Fraction.FractionComparisonOps.mkNumericOps
import Fraction.FractionComparisonOps.mkOrderingOps

object Fraction:
  private def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a%b)
  private val memoRound = collection.mutable.Map[Fraction, Fraction]()
  def sternBrocot(q: Fraction, l: Fraction = new Fraction(0, 1), h: Fraction = new Fraction(1, 0), i: Int = 0): Fraction =
    if (memoRound contains q) memoRound(q)
    else
      val med = (l.num + h.num) ¦ (l.den + h.den)
      if (i > 1000 || (med.toDouble - q.toDouble).abs < 0.000001)
        memoRound += q -> med
        med
      else if (med > q) sternBrocot(q, l, med, i+1)
      else sternBrocot(q, med, h, i+1)

  def apply(n: Long, d: Long): Fraction =
    val g = gcd(n,d)
    val a_ = n / g
    val b_ = d / g
    val (a,b) = if (b_ < 0) (-a_, -b_) else (a_, b_)
    if (a / Int.MaxValue != 0 || b / Int.MaxValue != 0)
      val useA = if (a < 0) -a else a
      val startPoint = 1l ¦ (b / useA)
      val q = new Fraction(useA,b)
      if (a < 0) -sternBrocot(q, h=startPoint) else sternBrocot(q, h=startPoint)
    else new Fraction(a, b)

  def unapply(f: Fraction): Option[(Int, Int)] =
    if (f.den != 0) Some(f.num.toInt, f.den.toInt) else None

  implicit object FractionComparisonOps extends Fractional[Fraction]:
    def compare(x: Fraction, y: Fraction): Int =
      val gc = gcd(x.den, y.den)
      (x.num * (y.den / gc)) compare (y.num * (x.den / gc))
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


extension (i: Int) def ¦(that: Int) = Fraction(i, that)
extension (i: Long) def ¦(that: Long) = Fraction(i, that)

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
