import sbt._
import java.util.jar.{Manifest}
import java.io.{FileInputStream}
import AdditionalResources._
/**
 * Additional tasks that are required to obtain a complete compiler and library pair, but that are not part of the
 * compilation task. It copies additional files and generates the properties files
 * @author Grégory Moix
 */
trait AdditionalResources {
  self : BasicLayer  =>
  
  lazy val copyAdditionalFiles = task {
      def copy0(steps:List[Step]):Option[String]= steps match{
        case x::xs => x match{
          case c:ResourcesToCopy => {
            c.copy orElse copy0(xs)
          }
          case _ => copy0(xs)
        }
        case Nil => None
      }
     copy0(allSteps.topologicalSort)
  }.dependsOn(externalCompilation)

  lazy val writeProperties = task {
      def write0(steps:List[Step]):Option[String]= steps match{
        case x::xs => x match{
        case c:PropertiesToWrite => {
            c.writeProperties orElse write0(xs)
          }
          case _ => write0(xs)
        }
        case Nil => None
      }
     write0(allSteps.topologicalSort)
  }.dependsOn(externalCompilation)


  

}

import sbt._
import BasicLayer._
object AdditionalResources {
  /**
   * A FileFilter that defines what are the files that will be copied
   */
  lazy val basicFilter =  "*.tmpl" | "*.xml"| "*.js"| "*.css" | "*.properties" | "*.swf" | "*.png"
  
}

trait ResourcesToCopy {
  self : CompilationStep =>
  def getResources(from:Path,filter:FileFilter):PathFinder = (from ##)** filter
  def getResources(from:Path):PathFinder = getResources(from,AdditionalResources.basicFilter)
  
  def copyDestination:Path
  def filesToCopy:PathFinder
  def copy = {
    try{
      FileUtilities.copy(filesToCopy.get,copyDestination,log)
    }catch{
      case e=>Some(e.toString)
    }
    None
  }
}

trait PropertiesToWrite {
  self : CompilationStep =>

  def propertyList:List[Tuple2[String,String]]
  def propertyDestination:Path

  def writeProperties:Option[String]={
    import java.io._
    import java.util.Properties
    
    val properties = new Properties

    def insert(list:List[Tuple2[String,String]]):Unit=list match{
      case Nil => 
      case x::xs => {
        properties setProperty(x._1, x._2)
        insert(xs)
      }
    }
    
    try{
      insert(propertyList)
      val destFile = propertyDestination.asFile
      val stream = new FileOutputStream(destFile)
      properties.store(stream, null)
    }catch{
      case e => Some(e.toString)
    }
    None
  }

}

