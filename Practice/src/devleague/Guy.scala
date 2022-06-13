package devleague
import es.tmoor.scanvas._, rendering._, BoundingBox._
import org.scalajs.dom.{KeyCode, document, HTMLElement, html}
import Fraction.FractionComparisonOps.{mkNumericOps, mkOrderingOps}
import Game._

object Guy extends SubTemplate with Gravitational {
  val children: Seq[Template] = Nil
  val (px,py,pw,ph) = parent.bounds.extract

  val timeScale = (tick*20) ¦ 139
  // println(s"Guy timescale is $timeScale")

  def checkForJump(): Boolean = 
    for (l@Line(x1,y1,x2,y2) <- borders.bottom if !(l.exempt contains this.uid))
      def vInLine = y + h == y1
      def hInLine = x + w > x1 && x < x2
      if (vInLine && hInLine)
        if (keys(KeyCode.Up))
          onGround = false
          dy = gravity * -6
        else
          onGround = true
          dy = 0¦1
        return true
    false

  val w = (26¦1) / pw
  val h = (42¦1) / ph
  x = 1¦2
  y = 1¦2

  def relativeBounds = BoundingBox(x,y,w,h)
  // println(s"Relative bounds of guy: $relativeBounds")
  // println(s"Global bounds of guy: $bounds")

  val intContent = """XY(Nf3t!:;`1rZl*N$w&DN`Zm!IW*vE>0>dmaEa`9s['/x2(zqs*f@9p`Z9|LvtvH2$mF|eKU)7YU[__GSQn_W%qW(37z[[PMtg@irf-1r"1v9-\[xWlG6 D6Ce^`AzDVXqbD)D_iS%2{\;!Ejs]p{5?4D[clD%}?28<% rq% TtFabR68*nmLY"ZQsl/QdtBWX+} H8o5m-J}rI*Sh:n!]Id5WI0&ri{>b7L=f,@Ss|[HaNw=E"""

  val img = EncImage.decode(intContent)

  def moveX(): Boolean = {
    val decrease =
      if (onGround) (1¦1) - (1¦20) * timeScale else (1¦1) - (1¦200) * timeScale
    if (onGround && keys(KeyCode.Left) && !keys(KeyCode.Right)) {
      // println(s"Moving left, ${dx} => ${dx - gravity}")
      dx = (dx - gravity) min (dx * decrease)
      dx = dx max -terminal
    } else if (onGround && !keys(KeyCode.Left) && keys(KeyCode.Right)) {
      // println(s"Moving right, ${dx} => ${dx + gravity}")
      dx = (dx + gravity) max (dx * decrease)
      dx = dx min terminal
    } else if (onGround && dx.abs < (1¦10000) * timeScale) dx = 0¦1
    else dx *= decrease
    return true
  }
  
  def sIdent[T](i: => T) = i
  var ld: (=> Unit) => _ = sIdent
  def drawSkin() = {    
    ld = if (dx > (0¦1)) sIdent else if (dx < (0¦1)) context.withFlip else ld
    ld {
      img(context)
    }
  }

  def draw(): Unit = {
    // println(s"Guy @ $bounds")
    drawSkin()
  }
}