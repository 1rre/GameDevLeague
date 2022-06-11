package es.tmoor.scanvas
import rendering.Context
import BoundingBox._

abstract class BaseTemplate {
  abstract class SubTemplate extends Template(this, context)
  def relativeBounds: BoundingBox
  def bounds: BoundingBox
  final def x0 = relativeBounds._1
  final def y0 = relativeBounds._2
  final def width = relativeBounds._3
  final def height = relativeBounds._4
  def context: Context
  def tick: Double
  def children: Seq[Template]
  def draw(): Unit
  final def render(): Unit = {
    context.withOffset(x0, y0) {
      println(s"Offset $x0, $y0")
      context.withScale(width, height) {
        println(s"Scale $width, $height")
        draw()
        children.foreach(_.render())
      }
    }
  }
}
abstract class Template(val parent: BaseTemplate, val context: Context) extends BaseTemplate {
  def bounds: BoundingBox = {
    val (px,py,pw,ph) = parent.bounds
    val (x,y,w,h) = relativeBounds
    (px+x*pw, py+y*ph, w*pw, h*ph)
  }
  def tick = parent.tick
}