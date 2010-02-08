/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

// $Id$

package scala.tools.partest
package nest

import java.io.File
import scala.tools.nsc.Settings
import scala.tools.nsc.io.{ Path, Directory }

abstract class TestFile(kind: String) {
  def file: File
  def fileManager: FileManager
  def createOutDir: Boolean
	
  val dir = file.getParentFile
  val dirpath = dir.getAbsolutePath
  val fileBase: String = basename(file.getName)
  def objDir = fileBase + "-" + kind + ".obj"

  // @mutates settings
  protected def baseSettings(settings: Settings) {
    settings appendToClasspath dirpath
    
    if (createOutDir)
      settings.outdir.value = (Path(dir) / objDir).createDirectory(true).path
    
    // add additional flags found in 'testname.flags'
    def flagsPath = Path(dir) / (fileBase + ".flags")
    flagsPath ifFile { _.slurp().trim } foreach (settings processArgumentString _)
  }

  def defineSettings(settings: Settings) {
    baseSettings(settings)
    settings appendToClasspath fileManager.CLASSPATH
  }

  private def basename(name: String): String = {
    val inx = name.lastIndexOf(".")
    if (inx < 0) name else name.substring(0, inx)
  }

  override def toString(): String = kind+" "+file
}

case class PosTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("pos")
case class NegTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("neg")
case class RunTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("run") 
case class BuildManagerTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("bm")
case class ScalaCheckTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("scalacheck")
case class JvmTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("jvm")
case class ShootoutTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("shootout") {
  override def defineSettings(settings: Settings) {
    super.defineSettings(settings)
    settings.outdir.value = file.getParent
  }
}
case class ScalapTestFile(file: File, fileManager: FileManager, createOutDir: Boolean) extends TestFile("scalap") {
  override def defineSettings(settings: Settings) {
    super.defineSettings(settings)
    settings.outdir.value = file.getParent
  }
}
