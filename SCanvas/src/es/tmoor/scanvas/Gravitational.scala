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

  override def updateEnv() = {
    val cx = x
    val cy = y
    val nx = x + dx
    val ny = y + dy
    
    if ((nx in (0¦1, 1¦1 - w)) && (ny in (0¦1, 1¦1 - h)))
      x = nx
      y = ny
      dy = terminal min dy + gravity
      println(s"Next: $nx, $ny")
  }
}