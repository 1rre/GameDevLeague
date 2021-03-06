import javax.imageio.ImageIO

case class Region(colour: Int, pixels: Seq[(Int, Int)])


@main def main(path: String) =
  val img = ImageIO.read(java.io.File(path))
  img.getColorModel
  val regions = collection.mutable.Buffer[Region]()
  for (i <- 0 until img.getWidth; j <- 0 until img.getHeight)
    val c = img.getRGB(i, j)
    if (c != 0) regions += Region(c, Seq((i,j)))
  def mergeRegions(): Unit =
    for (i1 <- regions.indices; i2 <- regions.indices if regions.indices.contains(i2) && i1 < i2)
      val r1 = regions(i1)
      val r2 = regions(i2)
      def haveBorder(p1: (Int, Int))(p2: (Int, Int)): Boolean =
        (p1._1 - p2._1).abs + (p1._2 - p2._2).abs == 1 
      if (r1.colour == r2.colour && r1.pixels.map(haveBorder).exists(f => r2.pixels.exists(f))) {
        regions -= r1
        regions -= r2
        regions += Region(r1.colour, r1.pixels ++ r2.pixels)
      }
  var mr = collection.mutable.Buffer[Region]()
  while (mr != regions)
    mr = regions.clone()
    mergeRegions()

  case class BorderPixel(borders: Byte, colour: Int, pos: (Int, Int))
  def getExterior(p: Seq[(Int, Int)]): Seq[(Int, (Int, Int))] =
    p.map {
      case (x,y) =>
        val m1 = if (!p.contains(x+1, y)) 8 else 0
        val m2 = if (!p.contains(x-1, y)) 4 else 0
        val m3 = if (!p.contains(x, y+1)) 2 else 0
        val m4 = if (!p.contains(x, y-1)) 1 else 0
        (m1 | m2 | m3 | m4, (x,y))
    }.filter(_._1 != 0)
  def getCorners(p: Seq[(Int, Int)]): Seq[(Int, Int)] =
    p.filter((x,y) => Seq((x+1, y), (x-1, y), (x, y+1), (x, y-1)).count(px => p.contains(px)) <= 2)
  // TODO: convert lines to path
  for (r <- regions) yield
    val ext = getExterior(r.pixels)
    println(ext)