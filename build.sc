import mill._, scalalib._, scalajslib._

object SCanvas extends ScalaJSModule {
  def scalaVersion = "3.1.1"
  def scalaJSVersion = "1.10.0"
  def scalacOptions = Seq("-deprecation")
  def ivyDeps = Agg(ivy"org.scala-js:scalajs-dom_sjs1_3:2.2.0")
}
object Practice extends ScalaJSModule {
  def scalaVersion = "3.1.1"
  def scalaJSVersion = "1.10.0"
  def scalacOptions = Seq("-deprecation")
  def ivyDeps = Agg(ivy"org.scala-js:scalajs-dom_sjs1_3:2.2.0")
  def moduleDeps = Seq(SCanvas)
}
object ImgCnv extends ScalaModule {
  def scalaVersion = "3.1.1"
}
object ImgWrite extends ScalaModule {
  def scalaVersion = "3.1.1"
}
