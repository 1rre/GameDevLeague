package es.tmoor.scanvas
import org.scalajs.dom.{html, raw, document}
import scala.scalajs.js.timers._
import rendering.Context
import BoundingBox._

abstract class SCanvas(web: html.Canvas, val tick: Int, width: Int, height: Int)
    extends BaseTemplate {
  web.width = width
  web.height = height
  def this(web: raw.Element, tick: Int, w: Int, h: Int) =
    this(web.asInstanceOf[html.Canvas], tick, w, h)
  def this(id: String, tick: Int, w: Int, h: Int) =
    this(document.getElementById(id), tick, w, h)

  val keys = collection.mutable.Set[Int]()
  
  document.onkeydown = {k =>
    //println(s"Add key ${k.key}")
    keys += k.keyCode
  }
  document.onkeyup = {k =>
    //println(s"Rm key ${k.key}")
    keys -= k.keyCode
  }

  val context = new Context(web)

  val borders = new Borders

  def bounds = BoundingBox(0¦1, 0¦1, width¦1, height¦1)
  def relativeBounds = BoundingBox(0¦1, 0¦1, 1¦1, 1¦1)

  def init(): Unit = {
    render()
    setInterval(tick)(render())
  }
}