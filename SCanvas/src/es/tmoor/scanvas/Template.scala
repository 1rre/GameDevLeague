package es.tmoor.scanvas
import rendering.Context
import BoundingBox._

import Fraction.FractionComparisonOps.mkNumericOps
import scala.reflect.ClassTag
import scala.reflect.TypeTest


abstract class BaseTemplate {
  val uid = util.Random.alphanumeric.take(32).mkString
  val blocks: collection.mutable.Set[Block[?]]
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
  val blocks = parent.blocks
}

type TemplateOrNot = BaseTemplate | None.type

type IsTemplate[T] = TypeTest[BaseTemplate, T]
class Block[T : IsTemplate](
    _left: => Fraction, _right: => Fraction, _top: => Fraction, _bottom: => Fraction,
    excludeSeq: Seq[String]
):
  def this(left: => Fraction, right: => Fraction, top: => Fraction, bottom: => Fraction) =
    this(left, right, top, bottom, Nil)
  override def toString(): String = s"Block($l, $r, $t, $b)"
  def l = _left
  def r = _right
  def t = _top
  def b = _bottom
  def excludes(elem: BaseTemplate): Boolean =
    (excludeSeq contains elem.uid) || (elem match {
      case _: T => true
      case _ => false
    })
type CollideAll = BaseTemplate & None.type