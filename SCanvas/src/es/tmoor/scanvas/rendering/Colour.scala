package es.tmoor.scanvas.rendering

object Colour {
  def apply(r: Int, g: Int, b: Int) = f"#$r%02x$g%02x$b%02x"
  def apply(r: Int, g: Int, b: Int, a: Int) = f"#$r%02x$g%02x$b%02x$a%02x"
  def apply(c: Int) = if (c > 0xFFFFFF) f"#$c%08x" else f"#$c%06x"
  val Transparent = Colour(0,0,0,0)
}
