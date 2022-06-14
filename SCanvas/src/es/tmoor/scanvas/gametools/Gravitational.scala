package es.tmoor.scanvas
package gametools

import BoundingBox._
import org.scalajs.dom.KeyCode
import math.Fractional.Implicits.infixFractionalOps
import math.Ordering.Implicits.infixOrderingOps
import java.awt.RenderingHints.Key

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
  def terminal = (25¦10000 * timeScale) min h
  def debug = false
  def moveAuto() = {
    if (dx != 0¦1 && dx.abs < 1¦1000000 * timeScale * timeScale)
      println(s"dx is low, setting to zero")
      dx = 0¦1
    if (!onGround)
      dy = terminal min (dy + gravity)
  }

  var onGround = false

  override def updateEnv() =
    val cx = x
    val cy = y
    val nx = x + dx
    val ny = y + dy
    onGround = false
    //if (dx != 0¦1) println(s"X : $cx (${cx.toDouble}) => $nx (${nx.toDouble}) (${if (nx > cx) "right" else "left"})")
    
    x = nx
    y = ny
    val pdy = dy
    val pdx = dx
    for (b <- blocks if !(b excludes this))
      if (nx + w > b.l && nx < b.r && ny + h > b.t && ny < b.b)
        val isV = cx + w > b.l && cx < b.r
        val isH = cy + h > b.t && cy < b.b
        if (isH && !isV)
          if (cx >= b.r)
            x = b.r
          else if (cx <= b.l - w)
            x = b.l - w
          else
            ???
          println(s"Horizontal intersection for $this on ${b.owner}: $nx, ${nx+w} @ ${b.l} and ${b.r}. Set x to $x")
          dx = 0¦1
        else if (isV && !isH)
          if (cy >= b.b)
            y = y max b.b
          else if (cy <= b.t - h)
            onGround = true
            y = y min (b.t - h)
          else
            ???
          println(s"Vertical intersection for $this on ${b.owner}: $ny, ${ny+h} @ ${b.t} and ${b.b}. Set y to $y")
          dy = 0¦1
        else
          println(s"More analysis needed (corner)")
          val m = if (pdx != 0¦1) pdy/pdx else 1¦0
          if (m > 0¦1)
            if (dx > 0¦1)
              println(s"Quartile: top left")
            else
              println(s"Quartile: bottom right")
          else
            if (dx > 0¦1)
              println(s"Quartile: top right")
            else
              println(s"Quartile: bottom left")
          println(s"Gradient is $m (${m.toDouble})")
      onGround |= cy + h == b.t && cx + w > b.l && cx < b.r
    moveAuto()
}