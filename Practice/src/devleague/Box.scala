package devleague
import es.tmoor.scanvas._, rendering._, BoundingBox._
import org.scalajs.dom.{KeyCode, document, HTMLImageElement, html}
import Fraction.FractionComparisonOps.mkNumericOps
import Game._

class Box(pxW: Fraction, pxH: Fraction, sx: Fraction, sy: Fraction) extends SubTemplate with Gravitational:
  val timeScale = (tick*20) ¦ 139
  val children: Seq[Template] = Nil
  val img = scalajs.js.eval("new Image(17, 17);").asInstanceOf[HTMLImageElement]
  img.src = "box.png"
  def rmBorders() =
    for (b <- borders.left if b.exempt contains this.uid)
      borders.left -= b
    for (b <- borders.right if b.exempt contains this.uid)
      borders.right -= b
    for (b <- borders.top if b.exempt contains this.uid)
      borders.top -= b
    for (b <- borders.bottom if b.exempt contains this.uid)
      borders.bottom -= b
  def setBorders() =
    val l1 = Line(relativeBounds.x, relativeBounds.y, relativeBounds.x, relativeBounds.y + h)
    l1.exempt += this.uid
    borders.right += l1
    val l2 = Line(relativeBounds.x + relativeBounds.w, relativeBounds.y, relativeBounds.x + relativeBounds.w, relativeBounds.y + relativeBounds.h)
    l2.exempt += this.uid
    borders.left += l2
    val l3 = Line(relativeBounds.x, relativeBounds.y + relativeBounds.h, relativeBounds.x + relativeBounds.w, relativeBounds.y + relativeBounds.h)
    l3.exempt += this.uid
    borders.top += l3
    val l4 = Line(relativeBounds.x, relativeBounds.y, relativeBounds.x + relativeBounds.w, relativeBounds.y)
    l4.exempt += this.uid
    borders.bottom += l4
  def draw(): Unit = {
    rmBorders()
    setBorders()
    // println(s"Drawing box @ $bounds")
    context.Fill.image(img)
  }

  def moveX(): Boolean = false
  
  def checkForJump(): Boolean = onGround

  val (px,py,pw,ph) = parent.bounds.extract
  val w = pxW / pw
  val h = pxH / ph
  // println(s"Box dimensions: $w, $h ($pw, $ph)")
  x = sx
  y = sy
  def relativeBounds: BoundingBox = BoundingBox(x, y, w, h)
