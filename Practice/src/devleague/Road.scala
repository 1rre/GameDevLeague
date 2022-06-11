package devleague
import es.tmoor.scanvas._
import es.tmoor.scanvas.rendering._
import org.scalajs.dom.KeyCode
import Game._

object Road extends SubTemplate {
  def relativeBounds = (0, 0.9, 1, 0.1)
  val (x,y,w,h) = relativeBounds
  val fh = bounds._4
  val lineWidth = fh / 12
  val offset = fh / 2
  def draw(): Unit = {
    context.Fill.colour = Colour(0x565664)
    context.Fill.regularPoly(4, (0,0,1,1))
    context.Draw.dash = (lineWidth, 3*lineWidth)
    context.Draw.colour = Colour(0xfdffe9)
    context.Draw.thickness = lineWidth
    context.Draw.line(0, 0.5, 1, 0.5)
  }
  def children: Seq[Template] = Nil
}