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

  val intContent = BigInt("t654bgurc92pdkvva94un75wqm599xzivg72yp2vqdx5t7ea3w9yg2o0qmou99j0bw6nc3beso3ph3t5vdq7f0086u590p574o8smmq8l0a3xh28ky10cy14dlqakthxry5wkqun4c5wnpbhtkg84us8mp6fo19ip8bcvd95y3q6jtr9kbpd0oozf5rffokf5jn6ntw0g7rxdr2xa5joz8uykdb69j2hlj91mvbxztw2n78i5i1m6nnrjr8sspqafbuno3smzwmctsdpn304zp1v4um51nrwo621dt2jjn0qtbmkcn47", 36)
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

  var ld: (=> Unit) => _ = context.withFlip
  val img1 = document.getElementById("sprite")
  val img = img1.asInstanceOf[html.Object]
  println(s"Loading Image")
  println(img.contentDocument)
  def drawSkin() = {    
    val fn = if (dx > 0) context.withFlip else if (dx < 0) identity else ld
    ld = fn
    
    
    context.withOffset(0.5, 0) {
      img
    }
  }

  def draw(): Unit = {
    updatePos()
    drawSkin()
  }
}