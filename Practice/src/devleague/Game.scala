package devleague
import es.tmoor.scanvas._
import es.tmoor.scanvas.rendering._
import org.scalajs.dom.KeyCode


object Game extends SCanvas("game", 1000/144, 640, 480) {
  val backgroundColour = Colour(0xbff2fd)
  def draw(): Unit = {
    context.Fill.colour = backgroundColour
    context.Fill.regularPoly(4, (0,0,640,480))
  }

  object Road extends SubTemplate {
    val (x,y,w,h) = bounds
    val lineWidth = h / 12
    val offset = h / 2
    def draw(): Unit = {
      context.Fill.colour = Colour(0x565664)
      context.Fill.regularPoly(4, (bounds))
      context.Draw.dash = (lineWidth, 3*lineWidth)
      context.Draw.colour = Colour(0xfdffe9)
      context.Draw.thickness = lineWidth
      context.Draw.line(x, y+offset, x+w, y+offset)
    }
    def children: Seq[Template] = Nil
    def relativeBounds = (0, 0.9, 1, 0.1)
  }

  object Guy extends SubTemplate {
    def t = Colour(0,0,0,0)  // Transparent
    def s = Colour(0xceba99) // Skin
    def r = Colour(0xeb2d1d) // Red
    def j = Colour(0x523ab5) // Jeans
    def b = Colour(0)        // Black
    def e = Colour(0xffffff) // Eye
    def baseLine = Road.y + Road.offset
    def children: Seq[Template] = Nil
    val (px,py,pw,ph) = parent.bounds
    val w = 13 * 2
    val h = 21 * 2
    var x = px + pw/2 - w/2
    var y = py + ph/2 - h/2

    var dy = 0d
    var dx = 0d

    val gravity = 0.02d
    val terminal = 2.5d
    
    def intersectRoad = y > baseLine - h
    def shouldFall = y < baseLine - h
    def canMove = y == baseLine - h

    def setDx(): Unit = {
      val decrease = if (canMove) 0.95d else 0.995d
      if (canMove) {
        if (keys(KeyCode.Left) && !keys(KeyCode.Right)) {
          println(s"Moving left, ${dx} => ${dx - gravity}")
          dx = (dx - gravity) min (dx * decrease)
          dx = dx max -terminal
        } else if (!keys(KeyCode.Left) && keys(KeyCode.Right)) {
          println(s"Moving right, ${dx} => ${dx + gravity}")
          dx = (dx + gravity) max (dx * decrease)
          dx = dx min terminal
        } else dx *= decrease
      } else dx *= decrease // Wind resistance?
    }

    def setDy(): Unit = {
      if (canMove) {
        if (keys(KeyCode.Up)) dy = -terminal
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
      x += dx
      y += dy
      if (x < 0) {
        x = 0
        if (canMove) dx = 0
        else dx *= -0.4
      } else if (x > 640 - w) {
        if (canMove) dx = 0
        else dx *= -0.4
        x = 640 - w
      }
      setDy()
      setDx()
    }
    // TODO: Setup mirroring
    def drawSkin() = {
      val head = Seq (
        (x + w * 4/13d, y),
        (x + w * 9/13d, y),
        (x + w * 9/13d, y + h * 2/21d),
        (x + w * 10/13d, y + h * 2/21d),
        (x + w * 10/13d, y + h * 3/21d),
        (x + w * 9/13d, y + h * 3/21d),
        (x + w * 9/13d, y + h * 5/21d),
        (x + w * 7/13d, y + h * 5/21d),
        (x + w * 7/13d, y + h * 6/21d),
        (x + w * 6/13d, y + h * 6/21d),
        (x + w * 6/13d, y + h * 5/21d),
        (x + w * 4/13d, y + h * 5/21d),
      )
      val arm1 = Seq (
        (x + w, y + h * 7/21d),
        (x + w, y + h * 15/21d),
        (x + w * 11/13d, y + h * 15/21d),
        (x + w * 11/13d, y + h * 14/21d),
        (x + w * 12/13d, y + h * 14/21d),
        (x + w * 12/13d, y + h * 12/21d),
        (x + w * 11/13d, y + h * 12/21d),
        (x + w * 11/13d, y + h * 8/21d),
        (x + w * 12/13d, y + h * 8/21d),
        (x + w * 12/13d, y + h * 7/21d),
      )
      
      val arm2 = Seq (
        (x, y + h * 7/21d),
        (x, y + h * 15/21d),
        (x + w * 2/13d, y + h * 15/21d),
        (x + w * 2/13d, y + h * 14/21d),
        (x + w * 1/13d, y + h * 14/21d),
        (x + w * 1/13d, y + h * 12/21d),
        (x + w * 2/13d, y + h * 12/21d),
        (x + w * 2/13d, y + h * 8/21d),
        (x + w * 1/13d, y + h * 8/21d),
        (x + w * 1/13d, y + h * 7/21d),
      )
      
      context.Fill.colour = s
      context.Fill.pointsd(head)
      context.Fill.pointsd(arm1)
      context.Fill.pointsd(arm2)

    }

    def draw(): Unit = {
      updatePos()
      drawSkin()
    }
    
    def relativeBounds = (x/pw,y/ph,w/pw,h/ph)
  }


  def children: Seq[Template] = Seq(Road, Guy)
  println("Initialised")
}