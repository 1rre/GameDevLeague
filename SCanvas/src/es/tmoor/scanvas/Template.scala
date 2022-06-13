package es.tmoor.scanvas
import rendering.Context
import BoundingBox._

import Fraction.FractionComparisonOps.mkNumericOps


case class Line(x1: Fraction, y1: Fraction, x2: Fraction, y2: Fraction):
  val exempt = collection.mutable.Set[String]()
class Borders:
  val left = collection.mutable.Set[Line]()
  val right = collection.mutable.Set[Line]()
  val top = collection.mutable.Set[Line]()
  val bottom = collection.mutable.Set[Line]()

abstract class BaseTemplate {
  val uid = util.Random.alphanumeric.take(32).mkString
  val borders: Borders
  abstract class SubTemplate extends Template(this, context)
  def relativeBounds: BoundingBox
  def bounds: BoundingBox
  val keys: collection.mutable.Set[Int]
  final def x0 = relativeBounds._1
  final def y0 = relativeBounds._2
  final def width = relativeBounds._3
  final def height = relativeBounds._4
  def context: Context
  def tick: Int
  val children: Seq[Template]
  def draw(): Unit
  def updateEnv(): Unit = {}
  final def render(): Unit = {
    updateEnv()
    context.withOffset(x0, y0) {
      context.withScale(width, height) {
        draw()
        children.foreach(_.render())
      }
    }
  }
}
abstract class Template(val parent: BaseTemplate, val context: Context) extends BaseTemplate {
  def bounds: BoundingBox = {
    val (px,py,pw,ph) = parent.bounds.extract
    val (x,y,w,h) = relativeBounds.extract
    BoundingBox(px+x*pw, py+y*ph, w*pw, h*ph)
  }
  val keys: collection.mutable.Set[Int] = parent.keys
  def tick = parent.tick
  val borders: Borders = parent.borders
}