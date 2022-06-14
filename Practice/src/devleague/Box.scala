package devleague
import es.tmoor.scanvas._, rendering._, BoundingBox._, gametools._
import org.scalajs.dom.{KeyCode, document, HTMLImageElement, html}
import Fraction.FractionComparisonOps.mkNumericOps
import Game._

class Box(pxW: Fraction, pxH: Fraction, sx: Fraction, sy: Fraction) extends SubTemplate with Gravitational:
  val timeScale = (tick*20) ¦ 139
  val children: Seq[Template] = Nil
  val img = scalajs.js.eval("new Image(17, 17);").asInstanceOf[HTMLImageElement]
  img.src = "box.png"
  val asBox = this
  
  val blocker = new Block[this.type](x, x + w, y, y + w) {
    override def owner: String = s"$asBox"
  }
  blocks += blocker
  def draw(): Unit = {
    // println(s"Drawing box @ $bounds")
    context.Fill.image(img)
  }

  val (px,py,pw,ph) = parent.bounds.extract
  val w = pxW / pw
  val h = pxH / ph
  // println(s"Box dimensions: $w, $h ($pw, $ph)")
  x = sx
  y = sy
  def relativeBounds: BoundingBox = BoundingBox(x, y, w, h)
