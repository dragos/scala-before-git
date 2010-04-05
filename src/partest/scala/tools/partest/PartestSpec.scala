/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package partest

import Properties._
import nsc.io._
import nsc.util.{ CommandLine, CommandLineSpec }

/** This takes advantage of bits of scala goodness to fully define a command
 *  line program with a minimum of duplicated code.  When the specification object
 *  is created, the vals are evaluated in order and each of them side effects
 *  a private accumulator.  What emerges is a full list of the valid unary
 *  and binary arguments, as well as autogenerated help.
 */
trait PartestSpec extends CommandLineSpec {
  override def isPassthroughProperty(key: String) = key == "partest.options"
  override def isSysPropOption(key: String) = {
    val segments = (key split '.').toList
    if (segments.size == 2 && segments.head == "partest") Some(segments.last)
    else None
  }

  private var _testKinds: List[String] = Nil
  private def kind(s: String) = returning(s)(_testKinds +:= _)

  def testKinds   = _testKinds
  def versionMsg  = Properties.versionMsg
  
  help("""
    |Usage: partest [<options>] [<test> <test> ...]
    |  <test>: a path to a test designator, typically a .scala file or a directory.
    |          Examples: files/pos/test1.scala, files/res/bug785""")
  
  heading             ("Test categories:")
  val isAll         = ("all"                / "run all tests (default, unless no options given)" ?)
                      (kind("pos")          / "Compile files that are expected to build" ?)
                      (kind("neg")          / "Compile files that are expected to fail" ?)
                      (kind("run")          / "Test JVM backend" ?)
                      (kind("jvm")          / "Test JVM backend" ?)
                      (kind("res")          / "Run resident compiler scenarii" ?)
                      (kind("buildmanager") / "Run Build Manager scenarii" ?)
                      (kind("scalacheck")   / "Run Scalacheck tests" ?)
                      (kind("script")       / "Run script files" ?)
                      (kind("shootout")     / "Run shootout tests" ?)
                      (kind("scalap")       / "Run scalap tests" ?)

  heading             ("""Test "smart" categories:""")
  val grepExpr      = "grep"        / "run all tests with a source file containing <expr>"    >>
  val isFailed      = "failed"      / "run all tests which failed on the last run"            ?
  
  heading             ("Specifying paths and additional flags, ~ means repository root:")
  val rootDir       = "rootdir"     / "path from ~ to partest (default: test)"                |> "test"
  val buildDir      = "builddir"    / "path from ~ to test build (default: build/pack)"       |> "build/pack"
  val srcDir        = "srcdir"      / "path from --rootdir to sources (default: files)"       |> "files"
  val javaOpts      = "javaopts"    / "flags to java on all runs (overrides JAVA_OPTS)"       |> envOrElse("JAVA_OPTS", "")
  val scalacOpts    = "scalacopts"  / "flags to scalac on all tests (overrides SCALAC_OPTS)"  |> envOrElse("SCALAC_OPTS", "")
  
                      ("pack"       / "alias for --builddir build/pack")                      ?> setProp("partest.build", "build/pack")
                      ("quick"      / "alias for --builddir build/quick")                     ?> setProp("partest.build", "build/quick")
  
  heading             ("Options influencing output:")
  val isTrace       = "trace"       / "show the individual steps taken by each test" ?
  val isShowDiff    = "show-diff"   / "show diff between log and check file"  ?
  val isShowLog     = "show-log"    / "show log on failures" ?
  val isDryRun      = "dry-run"     / "do not run tests, only show their traces." ?
  val isTerse       = "terse"       / "be less verbose (almost silent except for failures)" ?
  val isVerbose     = "verbose"     / "be more verbose (additive with --trace)" ?
  val isDebug       = "debug"       / "maximum debugging output" ?
  val isAnsi        = "ansi"        / "print output in color" ?
  
  heading             ("Other options:")
  val timeout       = "timeout"     / "Timeout in seconds"                                    >> ;
  val isCleanup     = "cleanup"     / "delete all stale files and dirs before run" ?
  val isNoCleanup   = "nocleanup"   / "do not delete any logfiles or object dirs" ?
  val isStats       = "stats"       / "collect and print statistics about the tests" ?
  val isValidate    = "validate"    / "examine test filesystem for inconsistencies" ?
  val isVersion     = "version"     / "print version" ?
  
  // no help for anything below this line - secret options
  // mostly intended for property configuration.
  val runsets       = "runsets" |> ""
  val isNoAlarms    = ("noalarms" ?)
  val isInsideAnt   = ("is-in-ant" ?)
  val testWarning   = "test-warning" |> "90"
}

object PartestSpecReference extends PartestSpec {
  import CommandLineSpec._
  
  def parsed: CommandLine = null
  override def isReferenceSpec = true

  def unary   = unaryOptions
  def binary  = binaryOptions
  def allArgs = unary ++ binary
  
  def isunaryOption(s: String)   = unary contains toOpt(s)
  def isbinaryOption(s: String)  = binary contains toOpt(s)

  def main(args: Array[String]): Unit = println(bashCompletion("partest"))
  
  /** Append bash completion for partest to the given file.
   */
  def appendCompletionTo(f: File) = f appendAll bashCompletion("partest")
}

