package devleague
import es.tmoor.scanvas._, rendering._, gametools._
import org.scalajs.dom.KeyCode
import Fraction.FractionComparisonOps.{mkNumericOps, mkOrderingOps}

object Game extends SCanvas("game", 25, 640, 480) {
  blocks += Block[CollideAll](0¦1, 0¦1, 0¦1, 1¦1) // Left wall
  blocks += Block[CollideAll](1¦1, 1¦1, 0¦1, 1¦1) // Right wall
  blocks += Block[CollideAll](0¦1, 1¦1, 0¦1, 0¦1) // Top ceiling
  blocks += Block[CollideAll](0¦1, 1¦1, 19¦20, 19¦20) // Bottom floor
  val backgroundColour = Colour(0xbff2fd)
  def draw(): Unit = {
    context.Fill.colour = backgroundColour
    context.Fill.regularPoly(4, BoundingBox(0¦1, 0¦1, 1¦1, 1¦1))
  }

  val children: Seq[Template] =
    val nBlocks = 4
    val mBlock = 17
    var offset = 1¦9 + util.Random.between(1, mBlock + 1)¦1 / bounds.w
    def boxes(height: Fraction) =
      offset = 1¦9
      for (_ <- 1 to nBlocks * 2) yield
        val s = util.Random.between(1, mBlock + 1)¦1
        val sW = s / bounds.w
        offset += (util.Random.nextLong(sW.num + 1)¦sW.den)
        val sH = s / bounds.h
        println(s"Box of size: ${s}")
        val r = new Box(s, s, offset, height - (sH / 2))
        offset += sW
        r

    Road +: (boxes(1¦3) ++ boxes(2¦3)) :+ Guy
  // println("Initialised")
}