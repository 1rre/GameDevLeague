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

  val w = (26¦1) / pw
  val h = (42¦1) / ph
  x = 1¦2
  y = 1¦2

  def relativeBounds = BoundingBox(x,y,w,h)
  // println(s"Relative bounds of guy: $relativeBounds")
  // println(s"Global bounds of guy: $bounds")

  val intContent = """XY(Nf3t!:;`1rZl*N$w&DN`Zm!IW*vE>0>dmaEa`9s['/x2(zqs*f@9p`Z9|LvtvH2$mF|eKU)7YU[__GSQn_W%qW(37z[[PMtg@irf-1r"1v9-\[xWlG6 D6Ce^`AzDVXqbD)D_iS%2{\;!Ejs]p{5?4D[clD%}?28<% rq% TtFabR68*nmLY"ZQsl/QdtBWX+} H8o5m-J}rI*Sh:n!]Id5WI0&ri{>b7L=f,@Ss|[HaNw=E"""

  val img = EncImage.decode(intContent)
  
  def sIdent[T](i: => T) = i
  var ld: (=> Unit) => _ = sIdent
  def drawSkin() = {    
    ld = if (dx > (0¦1)) sIdent else if (dx < (0¦1)) context.withFlip else ld
    ld {
      img(context)
    }
  }

  def draw(): Unit = {
    if (keys(KeyCode.Left)) x -= gravity * 5
    if (keys(KeyCode.Right)) x += gravity * 5
    // println(s"Guy @ $bounds")
    drawSkin()
  }
}