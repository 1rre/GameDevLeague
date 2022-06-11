package devleague
import es.tmoor.scanvas._
import es.tmoor.scanvas.rendering._
import org.scalajs.dom.KeyCode
import Game._

object Guy extends SubTemplate {
  def t = Colour(0,0,0,0)  // Transparent
  def s = Colour(0xceba99) // Skin
  def r = Colour(0xeb2d1d) // Red
  def j = Colour(0x523ab5) // Jeans
  def b = Colour(0)        // Black
  def e = Colour(0xffffff) // Eye
  def baseLine = 0.95
  def children: Seq[Template] = Nil
  val (px,py,pw,ph) = parent.bounds

  val w = 13 * 2 / pw
  val h = 21 * 2 / ph
  var x = 0.5
  var y = 0.5

  def relativeBounds = (x,y,w,h)

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

  var ld = w
  def drawSkin() = {
    val w = if (dx > 0) Guy.w else if (dx < 0) -Guy.w else ld
    ld = w
    val head = Seq (
      (-5/26d, 0d),
      (5/26d, 0d),
      (5/26d, 2/21d),
      (7/26d, 2/21d),
      (7/26d, 3/21d),
      (5/26d, 3/21d),
      (5/26d, 5/21d),
      (1/26d, 5/21d),
      (1/26d, 6/21d),
      (-1/26d, 6/21d),
      (-1/26d, 5/21d),
      (-5/26d, 5/21d),
    ).map((a,b) => (a*w.sign, b))
      

    def arm(w: Double) = Seq(
      (x - w * 7/13d, y + h * 7/21d),
      (x - w * 7/13d, y + h * 15/21d),
      (x - w * 5/13d, y + h * 15/21d),
      (x - w * 5/13d, y + h * 14/21d),
      (x - w * 6/13d, y + h * 14/21d),
      (x - w * 6/13d, y + h * 12/21d),
      (x - w * 5/13d, y + h * 12/21d),
      (x - w * 5/13d, y + h * 8/21d),
      (x - w * 6/13d, y + h * 8/21d),
      (x - w * 6/13d, y + h * 7/21d),
    )
    val arm1 = arm(w)
    val arm2 = arm(-w)

    def leg(w: Double) = Seq (
      (x - w * 4/13d, y + h * 18/21d),
      (x - w * 1/13d, y + h * 18/21d),
      (x - w * 1/13d, y + h * 19/21d),
      (x - w * 4/13d, y + h * 19/21d),    
    )

    val leg1 = leg(w)
    val leg2 = leg(-w)
    
    context.Fill.colour = s
    context.withOffset(0.5, 0) {
      context.Fill.points(head)

    }
    /*
    context.Fill.pointsd(arm1)
    context.Fill.pointsd(arm2)
    context.Fill.pointsd(leg1)
    context.Fill.pointsd(leg2)
    */
  }

  def draw(): Unit = {
    updatePos()
    drawSkin()
  }
}