package devleague
import es.tmoor.scanvas._, BoundingBox._, rendering._
import org.scalajs.dom.KeyCode

object Game extends SCanvas("game", 25, 640, 480) {
  borders.left += Line(0¦1, 0¦1, 0¦1, 1¦1)
  borders.right += Line(1¦1, 0¦1, 1¦1, 1¦1)
  borders.top += Line(0¦1, 0¦1, 1¦1, 0¦1)
  borders.bottom += Line(0¦1, 19¦20, 1¦1, 19¦20)
  val backgroundColour = Colour(0xbff2fd)
  def draw(): Unit = {
    context.Fill.colour = backgroundColour
    context.Fill.regularPoly(4, BoundingBox(0¦1, 0¦1, 1¦1, 1¦1))
  }

  val children: Seq[Template] = Seq(Road, new Box(17¦1, 17¦1, 1¦9, 1¦2), new Box(17¦2, 17¦2, 2¦9, 1¦2), new Box(17¦3, 17¦3, 3¦9, 1¦2), Guy)
  // println("Initialised")
}