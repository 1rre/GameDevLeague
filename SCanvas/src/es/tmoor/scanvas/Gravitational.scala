package es.tmoor.scanvas
import BoundingBox._
import org.scalajs.dom.KeyCode
import math.Fractional.Implicits.infixFractionalOps
import math.Ordering.Implicits.infixOrderingOps

/*
 TODO:
  Plot a track of current position => next position, and see what is intersected first (also where it is intersected?)
  This will mean that we need to check whether we began intersecting from the side or from the top of a line
*/

trait Gravitational { this: Template =>
  def timeScale: Fraction

  def w: Fraction
  def h: Fraction
  var x: Fraction = 0¦1
  var y: Fraction = 0¦1

  var dy = 0¦1
  var dx = 0¦1

  def gravity = 35¦1000000 * timeScale * timeScale // acceleration is ²
  def terminal = 25¦10000 * timeScale

  var onGround = false
  def bounceOffWalls(): Boolean = {
    for (l@Line(x1,y1,x2,y2) <- borders.left if !(l.exempt contains this.uid))
      def vInLine = y + h > y1 && y < y2
      def hInLine = x + w > x1 && x < x2
      if (vInLine && hInLine)
        x = x1
        if (dx.num < 0) dx *= -2¦5
        return true
    for (l@Line(x1,y1,x2,y2) <- borders.right if !(l.exempt contains this.uid))
      def vInLine = y + h > y1 && y < y2
      def hInLine = x + w > x1 && x < x2
      // println(s"For border $x1,$y1 => $x2,$y2:\nv: $vInLine\nh: $hInLine\nPos: $x,$y,$w,$h")
      if (vInLine && hInLine)
        x = x1 - w
        if (dx.num > 0) dx *= -2¦5
        return true
    false
  }
  def moveX(): Boolean

  def setDx(): Unit = {
    bounceOffWalls() || moveX()
  }

  def checkForFloorIntersect(): Boolean =
    // Diagonals don't exist for now
    for (l@Line(x1,y1,x2,y2) <- borders.bottom if !(l.exempt contains this.uid))
      assert(y1 == y2)
      def vInLine = y + h > y1 && y < y1
      def hInLine = x + w > x1 && x < x2
      // println(s"Check $x1 => $x2 @ $y1\nV: $vInLine, H: $hInLine")
      if (vInLine && hInLine)
        println(s"Intersect @ $y1 (other @ ${y + h})")
        dy = 0¦1
        y = y1 - h
        onGround = true
        return true
    false

  def checkForJump(): Boolean

  def increaseFall(): Boolean =
    dy += gravity
    dy = dy min terminal
    // println(s"Increase fall speed to $dy (added $gravity)")
    true
  
  def setDy(): Unit = {
    checkForFloorIntersect() || checkForJump() || increaseFall()
  }

  override def updateEnv() = {
    // println(s"Falling! x: $x + $dx, y: $y + $dy")
    x += dx
    y += dy
    setDy()
    setDx()
  }
}