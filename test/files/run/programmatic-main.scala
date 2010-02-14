import scala.tools.nsc._
import io.Path

object Test
{
  val basedir = (Path(System.getProperty("scalatest.cwd")).parent / "lib").path
  val baseargs = Array("-bootclasspath", basedir + "scala-library.jar", "-cp", basedir + "scala-compiler.jar")
    
  def main(args: Array[String]): Unit = {
    Main process (baseargs ++ Array("-Xshow-phases"))
  }
}
