package devleague
import es.tmoor.scanvas._
import es.tmoor.scanvas.rendering._
import org.scalajs.dom.KeyCode


object Game extends SCanvas("game", 33, 640, 480) {
  val timeScale = tick / 6.95d
  val backgroundColour = Colour(0xbff2fd)
  def draw(): Unit = {
    context.Fill.colour = backgroundColour
    context.Fill.regularPoly(4, (0,0,1,1))
  }

  def children: Seq[Template] = Seq(Road, Guy)
  println("Initialised")
}