package es.tmoor.scanvas
package gametools
import scala.reflect.TypeTest

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
  def owner = "border"
  def excludes(elem: BaseTemplate): Boolean =
    (excludeSeq contains elem.uid) || (elem match {
      case _: T => true
      case _ => false
    })
type CollideAll = BaseTemplate & None.type