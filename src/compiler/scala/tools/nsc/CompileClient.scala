/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc

import java.io.{ BufferedReader, File, InputStreamReader, PrintWriter }
import Properties.fileEndings
import scala.tools.util.PathResolver
import io.Path
import util.ClassPath

/** The client part of the fsc offline compiler.  Instead of compiling
 *  things itself, it send requests to a CompileServer.
 */
class StandardCompileClient extends CompileSocketShared {
  lazy val compileSocket: CompileSocket = CompileSocket

  val versionMsg  = "Fast " + Properties.versionMsg
  var verbose     = false
  var version     = false
  var shutdown    = false

  /** Convert a filename to an absolute path */
  def absFileName(path: String) = new File(path).getAbsolutePath()

  /** Convert a sequence of filenames, separated by <code>File.pathSeparator</code>,
    * into absolute filenames.
    */
  def absFileNames(paths: String) = ClassPath.map(paths, absFileName)

  protected def normalize(args: Array[String]): (String, String) = {
    var i = 0
    val vmArgs = new StringBuilder
    var serverAdr = ""
    
    while (i < args.length) {
      val arg = args(i)
      if (fileEndings exists(arg endsWith _)) {
        args(i) = Path(arg).toAbsolute.path
      } else if (arg startsWith "-J") {
        //see http://java.sun.com/j2se/1.5.0/docs/tooldocs/solaris/javac.html#J
        vmArgs append " "+arg.substring(2)
        args(i) = ""
      } else if (arg == "-verbose") {
        verbose = true
      } else if (arg == "-version") {
        version = true
      } else if (arg == "-shutdown") {
        shutdown = true
      }
      i += 1
      
      if (i < args.length) {
        arg match {
          case "-classpath" | "-sourcepath" | "-bootclasspath" | "-extdirs" | "-d"  =>
            args(i) = PathResolver.makeAbsolute(args(i))
            i += 1
          case "-server"  =>
            serverAdr = args(i)
            args(i-1) = ""
            args(i) = ""
          case _          =>
        }
      }
    }
    (vmArgs.toString, serverAdr)
  }

  // used by class ant.FastScalac to skip exit statement in Ant.
  def main0(args0: Array[String]): Int = {
    val args = if (args0 contains "-d") args0 else Array("-d", ".") ++ args0
    val (vmArgs, serverAdr) = normalize(args)
    
    if (version) {
      Console println versionMsg
      return 0
    }
    if (verbose) {
      Console println args.mkString("[Server arguments: ", " ", "]")
      Console println "[VM arguments: %s]".format(vmArgs)
    }
    val socket =
      if (serverAdr == "") compileSocket.getOrCreateSocket(vmArgs, !shutdown)
      else Some(compileSocket.getSocket(serverAdr))
    
    val success = socket match {
      case Some(sock) => fscCompile(sock, args)
      case _          =>
        Console.println(
          if (shutdown) "[No compilation server running.]" else "Compilation failed."
        )
        shutdown
    }
    if (success) 1 else 0
  }
}

object CompileClient extends StandardCompileClient {
  def main(args: Array[String]): Unit =
    sys.exit(try main0(args) catch { case e: Exception => 1 })
}
