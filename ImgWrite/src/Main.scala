import java.awt.image.BufferedImage
import java.awt.{Shape, Polygon, Graphics2D, Color}
import javax.imageio.ImageIO

case class Path(colour: Int, points: (Int, Int)*):
  def draw(g: Graphics2D) =
    val shape = Polygon(points.map(_._1).toArray, points.map(_._2).toArray, points.length)
    g.setColor(Color(colour))
    g.fill(shape)

def maxIdx(i: Int): Int =
  31 to 0 by -1 find (x => (i & (1 << x)) != 0) map (_ + 1) getOrElse 0

class Image(paths: Path*):
  def draw(g: Graphics2D) =
    paths.foreach(_.draw(g))
  def write: BigInt =
    var x: BigInt = 1
    val longest = maxIdx(paths.map(_.points.map((a,b) => a max b).max).max)
    x <<= 8
    x |= (longest & 0xFF)
    for (p <- paths)
      x <<= 32
      x |= (p.colour & 0xFFFFFFFFL)
      x <<= 32
      x |= p.points.length
      for ((a,b) <- p.points)
        x <<= longest
        x |= a
        x <<= longest
        x |= b
    x

@main def main =
  val image = new BufferedImage(13, 21, BufferedImage.TYPE_INT_ARGB)
  val head = Path(-2768738, (4,0), (9,0), (9,2), (10,2), (10,3), (9,3), (9,5), (7,5), (7,6), (6,6), (6,5), (4,5))
  val arms = Path(-2768738, (2,15), (0,15), (0,7), (13,7), (13,15), (11,15), (11,14), (12,14), (12,12), (11,12), (11,9), (2,9), (2,12), (1,12), (1,14), (2,14))
  val leg1 = Path(-2768738, (7,17), (10,17), (10,19), (7,19))
  val leg2 = Path(-2768738, (3,17), (6,17), (6,19), (3,19))
  val jeans = Path(-11388235, (3,11), (10,11), (10,17), (7,17), (7,14), (6,14), (6,17), (3,17))
  val eyeWhite = Path(-263173, (5,1), (6,1), (6,2), (5,2))
  val eyeBlack = Path(-16777216, (6,1), (7,1), (7,2), (6,2))
  val mouth = Path(-16777216, (6,3), (9,3), (9,4), (6,4))
  val leftShoe = Path(-16777216, (2,19), (5,19), (5,18), (6,18), (6,21), (2,21))
  val rightShoe = Path(-16777216, (11,19), (8,19), (8,18), (7,18), (7,21), (11,21))
  val tshirt = Path(-1364707, (0,6), (13,6), (13,7), (12,7), (12,8), (11,8), (11,9), (10,9), (10,12), (8,12), (8,11), (5,11), (5,12), (3,12), (3,9), (2,9), (2,8), (1,8), (1,7), (0,7))

  val img = Image(head, arms, leg1, leg2, jeans, eyeWhite, eyeBlack, mouth, leftShoe, rightShoe, tshirt)
  img.draw(image.getGraphics.asInstanceOf[Graphics2D])
  val r = img.write
  println(r.toString)
  def b92(i: BigInt): String =
    if (i == 0) ""
    else
      val c = (i % 94 + 32).toChar
      s"${b92(i / 94)}$c"
  println(r)
  println(r.toByteArray.toSeq.length)
  ImageIO.write(image, "png", java.io.File("out.png"))
  java.io.FileOutputStream("out.bin").write(r.toByteArray)