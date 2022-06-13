package devleague
import es.tmoor.scanvas._, BoundingBox._, rendering._
import org.scalajs.dom.KeyCode
import Game._
import Fraction.FractionComparisonOps.mkNumericOps

object Road extends SubTemplate {
  def relativeBounds = BoundingBox(0¦1, 9¦10, 1¦1, 1¦10)
  val (x,y,w,h) = relativeBounds.extract
  val fh = bounds._4
  val lineWidth = fh / 12
  val offset = fh / 2
  def draw(): Unit = {
    context.Fill.colour = Colour(0x565664)
    context.Fill.regularPoly(4, BoundingBox(0¦1,0¦1,1¦1,1¦1))
    context.Draw.dash = (lineWidth.toDouble, lineWidth.toDouble*3)
    context.Draw.colour = Colour(0xfdffe9)
    context.Draw.thickness = lineWidth.toDouble
    context.Draw.line(0, 0.5, 1, 0.5)
  }
  val children: Seq[Template] = Nil
}