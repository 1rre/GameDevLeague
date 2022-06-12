package devleague
import es.tmoor.scanvas._
import es.tmoor.scanvas.rendering._
import org.scalajs.dom.{KeyCode, document, HTMLElement, html}
import Game._

object Guy extends SubTemplate {
  def baseLine = Road.relativeBounds._2 + Road.relativeBounds._4 / 2d
  def children: Seq[Template] = Nil
  val (px,py,pw,ph) = parent.bounds

  val w = 13 * 2 / pw
  val h = 21 * 2 / ph
  var x = 0.5
  var y = 0.5

  def relativeBounds = (x,y,w,h)

  val intContent = """XY(Nf3t!:;`1rZl*N$w&DN`Zm!IW*vE>0>dmaEa`9s['/x2(zqs*f@9p`Z9|LvtvH2$mF|eKU)7YU[__GSQn_W%qW(37z[[PMtg@irf-1r"1v9-\[xWlG6 D6Ce^`AzDVXqbD)D_iS%2{\;!Ejs]p{5?4D[clD%}?28<% rq% TtFabR68*nmLY"ZQsl/QdtBWX+} H8o5m-J}rI*Sh:n!]Id5WI0&ri{>b7L=f,@Ss|[HaNw=E"""

  val img = EncImage.decode(intContent)
  var dy = 0d
  var dx = 0d

  val gravity = 0.000035d * timeScale * timeScale // acceleration is ²
  val terminal = 0.0025d * timeScale
  
  def intersectRoad = y > baseLine - h
  def shouldFall = y < baseLine - h
  def canMove = y == baseLine - h

  def setDx(): Unit = {
    val decrease = if (canMove) 1 - 0.05 * timeScale else 1 - 0.005 * timeScale
    if (canMove) {
      if (keys(KeyCode.Left) && !keys(KeyCode.Right)) {
        println(s"Moving left, ${dx} => ${dx - gravity}")
        dx = (dx - gravity) min (dx * decrease)
        dx = dx max -terminal
      } else if (!keys(KeyCode.Left) && keys(KeyCode.Right)) {
        println(s"Moving right, ${dx} => ${dx + gravity}")
        dx = (dx + gravity) max (dx * decrease)
        dx = dx min terminal
      } else if (dx.abs < 0.0001 * timeScale) dx = 0
      else dx *= decrease
    } else dx *= decrease // Wind resistance?
  }

  def setDy(): Unit = {
    if (canMove) {
      if (keys(KeyCode.Up)) dy = -5 * gravity
      else dy = 0
    } else if (intersectRoad) {
      y = baseLine - h
      dy = 0
    } else if (shouldFall) {
      dy += gravity
      dy = dy min terminal
    }
  }

  def updatePos(): Unit = {
    println(y)
    x += dx
    y += dy
    if (x < 0) {
      x = 0
      if (canMove) dx = 0
      else dx *= -0.4
    } else if (x > 1 - w) {
      if (canMove) dx = 0
      else dx *= -0.4
      x = 1 - w
    }
    setDy()
    setDx()
  }
  
  def sIdent[T](i: => T) = i
  var ld: (=> Unit) => _ = sIdent
  def drawSkin() = {    
    ld = if (dx > 0) sIdent else if (dx < 0) context.withFlip else ld
    ld {
      img(context)
    }
  }

  def draw(): Unit = {
    updatePos()
    drawSkin()
  }
}