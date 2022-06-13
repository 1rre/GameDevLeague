package es.tmoor.scanvas

import Fraction.FractionComparisonOps.mkNumericOps

case class BoundingBox(x: Fraction, y: Fraction, w: Fraction, h: Fraction):
  def extract: (Fraction, Fraction, Fraction, Fraction) = (x,y,w,h)
  def doubles: (Double, Double, Double, Double) = (x.toDouble,y.toDouble,w.toDouble,h.toDouble)