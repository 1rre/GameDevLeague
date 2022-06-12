package es.tmoor.scanvas
import org.scalajs.dom.{document, HTMLElement, html}
import collection.mutable.Buffer

object EncImage:
  private implicit class Bstr(s: String):
    def base2: Int = java.lang.Long.parseLong(s, 2).toInt
  def decode(input: BigInt): rendering.Context => Unit =
    println(input.toString)
    var x = input.toString(2).tail
    val longest = x.take(8).base2
    x = x.drop(8)
    println(s"Longest: $longest")
    val paths = Buffer[(Int, Seq[(Int, Int)])]()
    while (x.length > 0)
      println(s"x: $x")
      val colour = x.take(32).base2
      println(s"Colour: $colour")
      x = x.drop(32)
      val length = x.take(32).base2
      println(s"Length: $length")
      x = x.drop(32)
      val pts = for (_ <- 0 until length) yield
        val a = x.take(longest).base2
        x = x.drop(longest)
        val b = x.take(longest).base2
        x = x.drop(longest)
        (a, b)
      paths += ((colour, (pts)))
    println(paths)
    (r: rendering.Context) => {
      for ((c, p) <- paths)
        r.Fill.colour = c
        r.Fill.points(p.map((a,b) => (a/13d, b/21d)))
    }
    


      

    