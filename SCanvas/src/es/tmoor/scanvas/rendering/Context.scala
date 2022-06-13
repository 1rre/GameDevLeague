package es.tmoor.scanvas
package rendering

import org.scalajs.dom.{html, HTMLElement}
import org.scalajs.dom.raw.{CanvasRenderingContext2D, CanvasGradient}
import BoundingBox._
import Fraction.FractionComparisonOps.mkNumericOps

class Context(val c2d: CanvasRenderingContext2D) {
  def this(cnv: html.Canvas) =
    this(cnv.getContext("2d").asInstanceOf[CanvasRenderingContext2D])

  type Colour = String
  private var cx = 0¦1
  private var cy = 0¦1
  private var cw = c2d.canvas.width ¦ 1
  private var ch = c2d.canvas.height ¦ 1

  // TODO: make this rotate around the current centre
  def withRotation(r: Double)(fn: => Unit) = {
    c2d.rotate(r)
    fn
    c2d.rotate(-r)
  }

  val stack = collection.mutable.Stack[Fraction]()

  def withFlip(fn: => Unit) = {
    val rcx = cx
    cx *= -1
    cx -= cw
    c2d.scale(-1, 1)
    fn
    c2d.scale(-1, 1)
    cx = rcx
  }
  
  def withOffset(offset: (Fraction, Fraction))(fn: => Unit) = {
    // println(s"Set offset $cx $cy + ${offset._1 * cw} ${offset._2 * ch} on $this")
    stack.push(cx)
    stack.push(cy)
    cx += offset._1 * cw
    cy += offset._2 * ch
    fn
    cy = stack.pop()
    cx = stack.pop()
    // println(s"Unset offset $cx $cy + ${offset._1 * cw} ${offset._2 * ch} on $this")
  }

  def withScale(scale: (Fraction, Fraction))(fn: => Unit) = {
    // println(s"Set scale $cw $ch * ${scale._1} ${scale._2} on $this")
    stack.push(cw)
    stack.push(ch)
    cw *= scale._1
    ch *= scale._2
    fn
    ch = stack.pop()
    cw = stack.pop()
    // println(s"Unset scale $cw $ch * ${scale._1} ${scale._2} on $this")
  }

  def background = c2d.canvas.style.background
  def background_=(s: String) = c2d.canvas.style.background = s
  object Text {
    def colour = c2d.fillStyle
    def colour_=(c: Colour) = c2d.fillStyle = c
    def colour_=(g: CanvasGradient) = c2d.fillStyle = g
    def font = c2d.font
    def font_=(f: String) = c2d.font = f
    def centred(s: String, bounds: BoundingBox) = {
      val (x, y, w, h) = bounds.doubles
      c2d.textAlign = "center"
      c2d.textBaseline = "middle"
      c2d.fillText(s, x + w / 2, y + h / 2)
    }
    def left(s: String, bounds: BoundingBox) = {
      val (x, y, w, h) = bounds.doubles
      c2d.textAlign = "left"
      c2d.textBaseline = "middle"
      c2d.fillText(s, x, y + h / 2)
    }
  }
  object Glow {
    def size = c2d.shadowBlur
    def size_=(d: Double) = c2d.shadowBlur = d
    def colour = c2d.shadowColor
    def colour_=(c: Colour) = c2d.shadowColor = c
  }
  object Fill {
    def colour = c2d.fillStyle
    def colour_=(c: Colour) = c2d.fillStyle = c
    def colour_=(g: CanvasGradient) = c2d.fillStyle = g
    def image(i: HTMLElement) = {
      c2d.imageSmoothingEnabled = false
      // println(s"Draw image $i @ $cx, $cy/$cw, $ch")
      c2d.drawImage(i, cx.toDouble, cy.toDouble, cw.toDouble, ch.toDouble)
    }
    def regularPoly(sides: Int, bounds: BoundingBox): Unit = {
      c2d.beginPath()
      Trace.regularPoly(sides, bounds)
      c2d.fill()
    }
    def circle(b: BoundingBox) = {
      c2d.beginPath()
      Trace.circle(b)
      c2d.fill()
    }
    def circle(r: Double, centre: (Double, Double)) = {
      c2d.beginPath()
      Trace.circle(r, centre)
      c2d.fill()
    }
    def roundedRect(r: Double, bounds: BoundingBox): Unit = {
      c2d.beginPath()
      Trace.roundedRect(r, bounds)
      c2d.fill()
    }
    def points(pts: Seq[(Double, Double)]): Unit = {
      c2d.beginPath()
      Trace.points(pts)
      c2d.fill()
    }
  }
  object Draw {
    def colour = c2d.strokeStyle
    def colour_=(c: Colour) = c2d.strokeStyle = c
    def colour_=(g: CanvasGradient) = c2d.strokeStyle = g
    def thickness = c2d.lineWidth
    def thickness_=(d: Double) = c2d.lineWidth = d
    def dash = c2d.getLineDash()
    def dash_=(onOff: (Double, Double)) = c2d.setLineDash(scalajs.js.Array(onOff._1, onOff._2))
    def dash_=(nil: Nil.type) = c2d.setLineDash(scalajs.js.Array())
    def dash_=(array: scalajs.js.Array[Double]) = c2d.setLineDash(array)
    
    def line(x1: Double, y1: Double, x2: Double, y2: Double) = {
      c2d.beginPath()
      Trace.line(x1, y1, x2, y2)
      c2d.closePath()
      c2d.stroke()
    }
    def circle(b: BoundingBox) = {
      c2d.beginPath()
      Trace.circle(b)
      c2d.stroke()
    }
    def circle(r: Double, centre: (Double, Double)) = {
      c2d.beginPath()
      Trace.circle(r, centre)
      c2d.stroke()
    }
    def regularPoly(sides: Int, bounds: BoundingBox): Unit = {
      c2d.beginPath()
      Trace.regularPoly(sides, bounds)
      c2d.stroke()
    }
    def roundedRect(r: Double, bounds: BoundingBox): Unit = {
      c2d.beginPath()
      Trace.roundedRect(r, bounds)
      c2d.stroke()
    }
  }
  object Gradient {
    def apply(
        x1: Double,
        y1: Double,
        x2: Double,
        y2: Double,
        points: (Double, Colour)*
    ) = {
      val grad = c2d.createLinearGradient(x1, y1, x2, y2)
      for ((offset, c) <- points) grad.addColorStop(offset, c)
      grad
    }
  }
  object Trace {
    def roundedRect(r: Double, bounds: BoundingBox): Unit = {
      ???
      val (x, y, w, h) = bounds.doubles
      c2d.moveTo(x, y + r)
      c2d.arc(x + r, y + r, r, math.Pi, 3 * math.Pi / 2)
      c2d.lineTo(x + w - r, y)
      c2d.arc(x + w - r, y + r, r, 3 * math.Pi / 2, 0)
      c2d.lineTo(x + w, y + h - r)
      c2d.arc(x + w - r, y + h - r, r, 0, math.Pi / 2)
      c2d.lineTo(x + r, y + h)
      c2d.arc(x + r, y + h - r, r, math.Pi / 2, math.Pi)
      c2d.moveTo(x, y + r)
      c2d.arc(x + r, y + r, r, math.Pi, 3 * math.Pi / 2)
    }
    def circle(input: BoundingBox): Unit = {
      val bounds = input.doubles
      ???
      val x = bounds._1 + bounds._3 / 2
      val y = bounds._2 + bounds._4 / 2
      val r = math.min(bounds._3 / 2, bounds._4 / 2)
      c2d.arc(x, y, r, 0, 2 * math.Pi)
    }
    def circle(r: Double, centre: (Double, Double)): Unit = {
      ???
      val (x, y) = centre
      c2d.arc(x, y, r, 0, 2 * math.Pi)
    }
    def line(x1: Double, y1: Double, x2: Double, y2: Double) = {
      val p1x = cx.toDouble + x1 * cw.toDouble
      val p1y = cy.toDouble + y1 * ch.toDouble
      val p2x = cx.toDouble + x2 * cw.toDouble
      val p2y = cy.toDouble + y2 * ch.toDouble
      // println(s"Line from $x1, $y1 => $x2, $y2 ($p1x, $p1y => $p2x, $p2y)")
      c2d.moveTo(p1x, p1y)
      c2d.lineTo(p2x, p2y)
    }
    def points(pts: Seq[(Double, Double)]): Unit = {
      val p1x = cx.toDouble + pts.head._1 * cw.toDouble
      val p1y = cy.toDouble + pts.head._2 * ch.toDouble
      c2d.moveTo(p1x, p1y)
      (pts.tail :+ pts.head).foreach {
        case (a,b) =>
          val p2x = cx.toDouble + a * cw.toDouble
          val p2y = cy.toDouble + b * ch.toDouble
          c2d.lineTo(p2x, p2y)
      }
    }
    def regularPoly(sides: Int, bounds: BoundingBox): Unit = {
      val (x_, y_, w_, h_) = bounds.doubles
      // println(s"Draw poly init/$sides, $x_, $y_, $w_, $h_ ($cx, $cy, $cw, $ch)")
      val x = cx.toDouble + x_ * cw.toDouble
      val y = cy.toDouble + y_ * ch.toDouble
      val w = w_ * cw.toDouble
      val h = h_ * ch.toDouble
      // println(s"Draw poly/$sides, $x, $y, $w, $h")
      val anglePerSide = 2 * math.Pi / sides
      var pos = (0d, 0d)
      var maxX = 0d
      var maxY = 0d
      var minX = 0d
      var minY = 0d
      var angle = 0d
      val localPoints = for (i <- 0 to sides + 1) yield {
        pos = (pos._1 + math.cos(angle), pos._2 + math.sin(angle))
        angle += anglePerSide
        if (pos._1 > maxX) maxX = pos._1
        if (pos._2 > maxY) maxY = pos._2
        if (pos._1 < minX) minX = pos._1
        if (pos._2 < minY) minY = pos._2
        pos
      }
      val points = localPoints.map {
        case (px, py) => {
          (
            x + w * ((maxX - px) / (maxX - minX)),
            y + h * ((maxY - py) / (maxY - minY))
          )
        }
      }
      c2d.moveTo(points.head._1, points.head._2)
      points.foreach { case (nextX, nextY) =>
        c2d.lineTo(nextX, nextY)
      }
    }
  }
}
