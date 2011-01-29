/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import Predef.{ println => _, _ }
import java.io.{ PrintWriter }
import java.lang.reflect
import java.net.URL
import util.{ Set => _, _ }
import io.VirtualDirectory
import reporters.{ ConsoleReporter, Reporter }
import symtab.{ Flags, Names }
import scala.tools.nsc.interpreter.{ Results => IR }
import scala.tools.util.PathResolver
import scala.tools.nsc.util.{ ScalaClassLoader, Exceptional }
import ScalaClassLoader.URLClassLoader
import Exceptional.unwrap

import scala.collection.{ mutable, immutable }
import scala.PartialFunction.{ cond, condOpt }
import scala.util.control.Exception.{ ultimately }
import scala.reflect.NameTransformer
import IMain._

/** <p>
 *    An interpreter for Scala code.
 *  </p>
 *  <p>
 *    The main public entry points are <code>compile()</code>,
 *    <code>interpret()</code>, and <code>bind()</code>.
 *    The <code>compile()</code> method loads a
 *    complete Scala file.  The <code>interpret()</code> method executes one
 *    line of Scala code at the request of the user.  The <code>bind()</code>
 *    method binds an object to a variable that can then be used by later
 *    interpreted code.
 *  </p>
 *  <p>
 *    The overall approach is based on compiling the requested code and then
 *    using a Java classloader and Java reflection to run the code
 *    and access its results.
 *  </p>
 *  <p>  
 *    In more detail, a single compiler instance is used
 *    to accumulate all successfully compiled or interpreted Scala code.  To
 *    "interpret" a line of code, the compiler generates a fresh object that
 *    includes the line of code and which has public member(s) to export
 *    all variables defined by that code.  To extract the result of an
 *    interpreted line to show the user, a second "result object" is created
 *    which imports the variables exported by the above object and then
 *    exports a single member named "scala_repl_result".  To accomodate user expressions
 *    that read from variables or methods defined in previous statements, "import"
 *    statements are used.
 *  </p>
 *  <p>
 *    This interpreter shares the strengths and weaknesses of using the
 *    full compiler-to-Java.  The main strength is that interpreted code
 *    behaves exactly as does compiled code, including running at full speed.
 *    The main weakness is that redefining classes and methods is not handled
 *    properly, because rebinding at the Java level is technically difficult.
 *  </p>
 *
 * @author Moez A. Abdel-Gawad
 * @author Lex Spoon
 */
class IMain(val settings: Settings, out: PrintWriter) {
  repl =>  
  
  /** construct an interpreter that reports to Console */
  def this(settings: Settings) = this(settings, new NewLinePrintWriter(new ConsoleWriter, true))
  def this() = this(new Settings())

  /** whether to print out result lines */
  private[nsc] var printResults: Boolean = true
  
  /** whether to print errors */
  private[nsc] var totalSilence: Boolean = false

  private val RESULT_OBJECT_PREFIX = "RequestResult$"
  
  lazy val formatting: Formatting = new Formatting {
    val prompt = Properties.shellPromptString
  }
  import formatting._

  def println(x: Any) = {
    out.println(x)
    out.flush()
  }
  
  /** directory to save .class files to */
  val virtualDirectory = new VirtualDirectory("(memory)", None)
  
  /** reporter */
  object reporter extends ConsoleReporter(settings, null, out) {
    override def printMessage(msg: String) {
      if (totalSilence)
        return
        
      out println (
        if (truncationOK) clean(msg)
        else cleanNoTruncate(msg)
      )
      out.flush()
    }
  }

  /** We're going to go to some trouble to initialize the compiler asynchronously.
   *  It's critical that nothing call into it until it's been initialized or we will
   *  run into unrecoverable issues, but the perceived repl startup time goes
   *  through the roof if we wait for it.  So we initialize it with a future and
   *  use a lazy val to ensure that any attempt to use the compiler object waits
   *  on the future.
   */
  private val _compiler: Global = newCompiler(settings, reporter)
  private def _initialize(): Boolean = {
    val source = """
      |// this is assembled to force the loading of approximately the
      |// classes which will be loaded on the first expression anyway.
      |class $repl_$init {
      |  val x = "abc".reverse.length + (5 max 5)
      |  scala.runtime.ScalaRunTime.stringOf(x)
      |}
      |""".stripMargin
    
    try {
      new _compiler.Run() compileSources List(new BatchSourceFile("<init>", source))
      if (isReplDebug || settings.debug.value)
        println("Repl compiler initialized.")
      true
    } 
    catch {
      case x: AbstractMethodError =>
        println("""
          |Failed to initialize compiler: abstract method error.
          |This is most often remedied by a full clean and recompile.
          |""".stripMargin
        )
        x.printStackTrace()
        false
      case x: MissingRequirementError => println("""
        |Failed to initialize compiler: %s not found.
        |** Note that as of 2.8 scala does not assume use of the java classpath.
        |** For the old behavior pass -usejavacp to scala, or if using a Settings
        |** object programatically, settings.usejavacp.value = true.""".stripMargin.format(x.req)
      )
      false
    }
  }
  
  // set up initialization future
  private var _isInitialized: () => Boolean = null
  def initialize() = synchronized { 
    if (_isInitialized == null)
      _isInitialized = scala.concurrent.ops future _initialize()
  }

  /** the public, go through the future compiler */
  lazy val global: Global = {
    initialize()

    // blocks until it is ; false means catastrophic failure
    if (_isInitialized()) _compiler
    else null
  }
  @deprecated("Use `global` for access to the compiler instance.")
  lazy val compiler = global

  import global._
  import definitions.{ EmptyPackage, getMember }
  import nme.{ 
    INTERPRETER_VAR_PREFIX, INTERPRETER_SYNTHVAR_PREFIX, INTERPRETER_LINE_PREFIX,
    INTERPRETER_IMPORT_WRAPPER, INTERPRETER_WRAPPER_SUFFIX, USCOREkw
  }  

  /** Temporarily be quiet */
  def beQuietDuring[T](operation: => T): T = {    
    val wasPrinting = printResults    
    ultimately(printResults = wasPrinting) {
      printResults = false
      operation
    }
  }
  def beSilentDuring[T](operation: => T): T = {
    val saved = totalSilence
    totalSilence = true
    try operation
    finally totalSilence = saved
  }
  
  def quietRun[T](code: String) = beQuietDuring(interpret(code))

  /** whether to bind the lastException variable */
  private var bindLastException = true
  
  /** Temporarily stop binding lastException */
  def withoutBindingLastException[T](operation: => T): T = {
    val wasBinding = bindLastException
    ultimately(bindLastException = wasBinding) {
      bindLastException = false
      operation
    }
  }
  
  protected def createLineManager(): Line.Manager = new Line.Manager
  lazy val lineManager = createLineManager()

  /** interpreter settings */
  lazy val isettings = new ISettings(this)

  /** Instantiate a compiler.  Subclasses can override this to
   *  change the compiler class used by this interpreter. */
  protected def newCompiler(settings: Settings, reporter: Reporter) = {
    settings.outputDirs setSingleOutput virtualDirectory    
    new Global(settings, reporter)
  }
  
  /** the compiler's classpath, as URL's */
  lazy val compilerClasspath: List[URL] = new PathResolver(settings) asURLs

  /* A single class loader is used for all commands interpreted by this Interpreter.
     It would also be possible to create a new class loader for each command
     to interpret.  The advantages of the current approach are:

       - Expressions are only evaluated one time.  This is especially
         significant for I/O, e.g. "val x = Console.readLine"

     The main disadvantage is:

       - Objects, classes, and methods cannot be rebound.  Instead, definitions
         shadow the old ones, and old code objects refer to the old
         definitions.
  */
  private var _classLoader: AbstractFileClassLoader = null
  def resetClassLoader() = _classLoader = makeClassLoader()
  def classLoader: AbstractFileClassLoader = {
    if (_classLoader == null)
      resetClassLoader()
    
    _classLoader
  }
  private def makeClassLoader(): AbstractFileClassLoader = {
    val parent =
      if (parentClassLoader == null)  ScalaClassLoader fromURLs compilerClasspath
      else                            new URLClassLoader(compilerClasspath, parentClassLoader)

    new AbstractFileClassLoader(virtualDirectory, parent)
  }
  private def loadByName(s: String): Class[_] = (classLoader tryToInitializeClass s).get
  private def methodByName(c: Class[_], name: String): reflect.Method =
    c.getMethod(name, classOf[Object])
  
  protected def parentClassLoader: ClassLoader =
    settings.explicitParentLoader.getOrElse( this.getClass.getClassLoader() )

  def getInterpreterClassLoader() = classLoader

  // Set the current Java "context" class loader to this interpreter's class loader
  def setContextClassLoader() = classLoader.setAsContext()

  /** the previous requests this interpreter has processed */
  private val prevRequests      = new mutable.ArrayBuffer[Request]()
  private val referencedNameMap = new mutable.HashMap[Name, Request]()
  private val boundNameMap      = new mutable.HashMap[Name, Request]()
  private def allHandlers       = prevRequests.toList flatMap (_.handlers)
  private def allReqAndHandlers = prevRequests.toList flatMap (req => req.handlers map (req -> _))
  
  def printAllTypeOf = {
    prevRequests foreach { req =>
      req.typeOf foreach { case (k, v) => Console.println(k + " => " + v) }
    }
  }

  /** Most recent tree handled which wasn't wholly synthetic. */
  private def mostRecentlyHandledTree: Option[Tree] = {
    for {
      req <- prevRequests.reverse
      handler <- req.handlers.reverse
      name <- handler.generatesValue
      if !isSynthVarName(name)
    } return Some(handler.member)

    None
  }
  
  /** Stubs for work in progress. */
  def handleTypeRedefinition(name: TypeName, old: Request, req: Request) = {
    for (t1 <- old.simpleNameOfType(name) ; t2 <- req.simpleNameOfType(name)) {
      DBG("Redefining type '%s'\n  %s -> %s".format(name, t1, t2))
    }
  }

  def handleTermRedefinition(name: TermName, old: Request, req: Request) = {
    for (t1 <- old.compilerTypeOf get name ; t2 <- req.compilerTypeOf get name) {
      // Printing the types here has a tendency to cause assertion errors, like
      //   assertion failed: fatal: <refinement> has owner value x, but a class owner is required
      // so DBG is by-name now to keep it in the family.  (It also traps the assertion error,
      // but we don't want to unnecessarily risk hosing the compiler's internal state.)
      DBG("Redefining term '%s'\n  %s -> %s".format(name, t1, t2))
    }
  }

  def recordRequest(req: Request) {
    def tripart[T](set1: Set[T], set2: Set[T]) = {
      val intersect = set1 intersect set2
      List(set1 -- intersect, intersect, set2 -- intersect)
    }

    prevRequests += req
    req.referencedNames foreach (x => referencedNameMap(x) = req)
    
    req.boundNames foreach { name =>
      if (boundNameMap contains name) {
        if (name.isTypeName) handleTypeRedefinition(name.toTypeName, boundNameMap(name), req)
        else handleTermRedefinition(name.toTermName, boundNameMap(name), req)
      }
      boundNameMap(name) = req
    }

    // XXX temporarily putting this here because of tricky initialization order issues
    // so right now it's not bound until after you issue a command.
    if (prevRequests.size == 1)
      quietBind("settings", isettings)
  }
  
  private def keyList[T](x: collection.Map[T, _]): List[T] = x.keys.toList sortBy (_.toString)
  def allreferencedNames      = keyList(referencedNameMap)
  def allBoundNames           = keyList(boundNameMap)
  def allSeenTypes            = prevRequests.toList flatMap (_.typeOf.values.toList) distinct
  def allDefinedTypes         = prevRequests.toList flatMap (_.definedTypes.values.toList) distinct
  def allValueGeneratingNames = allHandlers flatMap (_.generatesValue)
  def allImplicits            = partialFlatMap(allHandlers) {
    case x: MemberHandler if x.definesImplicit => x.boundNames
  }

  /** Generates names pre0, pre1, etc. via calls to apply method */
  class NameCreator(pre: String) {
    private var x = -1
    var mostRecent: String = ""
    
    def apply(): String = { 
      x += 1
      val name = pre + x.toString
      // make sure we don't overwrite their unwisely named res3 etc.
      mostRecent =
        if (allBoundNames exists (_.toString == name)) apply()
        else name
      
      mostRecent
    }
    def reset(): Unit = x = -1
    def didGenerate(name: String) =
      (name startsWith pre) && ((name drop pre.length) forall (_.isDigit))
  }

  /** allocate a fresh line name */
  private lazy val lineNameCreator = new NameCreator(INTERPRETER_LINE_PREFIX)
  
  /** allocate a fresh var name */
  private lazy val varNameCreator = new NameCreator(INTERPRETER_VAR_PREFIX)
  
  /** allocate a fresh internal variable name */
  private lazy val synthVarNameCreator = new NameCreator(INTERPRETER_SYNTHVAR_PREFIX)

  /** Check if a name looks like it was generated by varNameCreator */
  private def isGeneratedVarName(name: String): Boolean = varNameCreator didGenerate name  
  private def isSynthVarName(name: String): Boolean = synthVarNameCreator didGenerate name
  private def isSynthVarName(name: Name): Boolean = synthVarNameCreator didGenerate name.toString
  
  def getVarName = varNameCreator()
  def getSynthVarName = synthVarNameCreator()

  /** Truncate a string if it is longer than isettings.maxPrintString */
  private def truncPrintString(str: String): String = {
    val maxpr = isettings.maxPrintString
    val trailer = "..."
    
    if (maxpr <= 0 || str.length <= maxpr) str
    else str.substring(0, maxpr-3) + trailer
  }

  /** Clean up a string for output */  
  private def clean(str: String) = truncPrintString(cleanNoTruncate(str))
  private def cleanNoTruncate(str: String) =
    if (isettings.unwrapStrings) stripWrapperGunk(str)
    else str  

  implicit def name2string(name: Name) = name.toString

  /** Compute imports that allow definitions from previous
   *  requests to be visible in a new request.  Returns
   *  three pieces of related code:
   *
   *  1. An initial code fragment that should go before
   *  the code of the new request.
   *
   *  2. A code fragment that should go after the code
   *  of the new request.
   *
   *  3. An access path which can be traverested to access
   *  any bindings inside code wrapped by #1 and #2 .
   *
   * The argument is a set of Names that need to be imported.
   *
   * Limitations: This method is not as precise as it could be.
   * (1) It does not process wildcard imports to see what exactly
   * they import.
   * (2) If it imports any names from a request, it imports all
   * of them, which is not really necessary.
   * (3) It imports multiple same-named implicits, but only the
   * last one imported is actually usable.
   */
  private case class ComputedImports(prepend: String, append: String, access: String)
  private def importsCode(wanted: Set[Name]): ComputedImports = {
    /** Narrow down the list of requests from which imports 
     *  should be taken.  Removes requests which cannot contribute
     *  useful imports for the specified set of wanted names.
     */
    case class ReqAndHandler(req: Request, handler: MemberHandler) { }
    
    def reqsToUse: List[ReqAndHandler] = {      
      /** Loop through a list of MemberHandlers and select which ones to keep.
        * 'wanted' is the set of names that need to be imported.
       */
      def select(reqs: List[ReqAndHandler], wanted: Set[Name]): List[ReqAndHandler] = {
        val isWanted = wanted contains _
        // Single symbol imports might be implicits! See bug #1752.  Rather than
        // try to finesse this, we will mimic all imports for now.
        def keepHandler(handler: MemberHandler) = handler match {
          case _: ImportHandler => true
          case x                => x.definesImplicit || (x.boundNames exists isWanted)
        }
                   
        reqs match {
          case Nil                                    => Nil
          case rh :: rest if !keepHandler(rh.handler) => select(rest, wanted)
          case rh :: rest                             =>
            val importedNames = rh.handler match { case x: ImportHandler => x.importedNames ; case _ => Nil }
            import rh.handler._
            val newWanted = wanted ++ referencedNames -- boundNames -- importedNames
            rh :: select(rest, newWanted)
        }
      }
      
      /** Flatten the handlers out and pair each with the original request */
      select(allReqAndHandlers reverseMap { case (r, h) => ReqAndHandler(r, h) }, wanted).reverse
    }

    val code, trailingBraces, accessPath = new StringBuilder
    val currentImps = mutable.HashSet[Name]()

    // add code for a new object to hold some imports
    def addWrapper() {
      val impname = INTERPRETER_IMPORT_WRAPPER
      code append "object %s {\n".format(impname)
      trailingBraces append "}\n"
      accessPath append ("." + impname)
      
      currentImps.clear
    }

    addWrapper()

    // loop through previous requests, adding imports for each one
    for (ReqAndHandler(req, handler) <- reqsToUse) {
      handler match {
        // If the user entered an import, then just use it; add an import wrapping
        // level if the import might conflict with some other import
        case x: ImportHandler =>
          if (x.importsWildcard || (currentImps exists (x.importedNames contains _)))
            addWrapper()
          
          code append (x.member.toString + "\n")
          
          // give wildcard imports a import wrapper all to their own
          if (x.importsWildcard) addWrapper()
          else currentImps ++= x.importedNames

        // For other requests, import each bound variable.
        // import them explicitly instead of with _, so that
        // ambiguity errors will not be generated. Also, quote 
        // the name of the variable, so that we don't need to 
        // handle quoting keywords separately. 
        case x =>
          for (imv <- x.boundNames) {
            if (currentImps contains imv) addWrapper()
        
            code append ("import %s\n" format (req fullPath imv))
            currentImps += imv
          }
      }
    }
    // add one extra wrapper, to prevent warnings in the common case of
    // redefining the value bound in the last interpreter request.
    addWrapper()
    ComputedImports(code.toString, trailingBraces.toString, accessPath.toString)
  }

  /** Parse a line into a sequence of trees. Returns None if the input is incomplete. */
  def parse(line: String): Option[List[Tree]] = {
    var justNeedsMore = false
    reporter.withIncompleteHandler((pos,msg) => {justNeedsMore = true}) {
      // simple parse: just parse it, nothing else
      def simpleParse(code: String): List[Tree] = {
        reporter.reset
        val unit = new CompilationUnit(new BatchSourceFile("<console>", code))
        val scanner = new syntaxAnalyzer.UnitParser(unit)
        
        scanner.templateStatSeq(false)._2
      }
      val trees = simpleParse(line)
      
      if (reporter.hasErrors)   Some(Nil)  // the result did not parse, so stop
      else if (justNeedsMore)   None
      else                      Some(trees)
    }
  }
  def isParseable(line: String): Boolean = {
    beSilentDuring {
      parse(line) match {
        case Some(xs) => xs.nonEmpty
        case _        => false
      }
    }
  }

  /** Compile an nsc SourceFile.  Returns true if there are
   *  no compilation errors, or false otherwise.
   */
  def compileSources(sources: SourceFile*): Boolean = {
    reporter.reset
    new Run() compileSources sources.toList
    !reporter.hasErrors
  }

  /** Compile a string.  Returns true if there are no
   *  compilation errors, or false otherwise.
   */
  def compileString(code: String): Boolean =
    compileSources(new BatchSourceFile("<script>", code))

  def compileAndSaveRun(label: String, code: String) = {
    /** Secret bookcase entrance for repl debuggers: end the line
     *  with "// show" and see what's going on.
     */
    if (code.lines exists (_.trim endsWith "// show")) {
      Console println code
      parse(code) match {
        case Some(trees)  => trees foreach (t => DBG(asCompactString(t)))
        case _            => DBG("Parse error:\n\n" + code)
      }
    }
    val run = new Run()
    run.compileSources(List(new BatchSourceFile(label, code)))
    run
  }

  /** Build a request from the user. <code>trees</code> is <code>line</code>
   *  after being parsed.
   */
  private def buildRequest(line: String, lineName: String, trees: List[Tree]): Request =
    new Request(line, lineName, trees)

  private def chooseHandler(member: Tree): MemberHandler = member match {
    case member: DefDef               => new DefHandler(member)
    case member: ValDef               => new ValHandler(member)
    case member@Assign(Ident(_), _)   => new AssignHandler(member)
    case member: ModuleDef            => new ModuleHandler(member)
    case member: ClassDef             => new ClassHandler(member)
    case member: TypeDef              => new TypeAliasHandler(member)
    case member: Import               => new ImportHandler(member)
    case DocDef(_, documented)        => chooseHandler(documented)
    case member                       => new GenericHandler(member)
  }
  
  private def requestFromLine(line: String, synthetic: Boolean): Either[IR.Result, Request] = {
    val trees = parse(indentCode(line)) match {
      case None         => return Left(IR.Incomplete)
      case Some(Nil)    => return Left(IR.Error) // parse error or empty input
      case Some(trees)  => trees
    }
    
    // use synthetic vars to avoid filling up the resXX slots
    def varName = if (synthetic) getSynthVarName else getVarName

    // Treat a single bare expression specially. This is necessary due to it being hard to
    // modify code at a textual level, and it being hard to submit an AST to the compiler.
    if (trees.size == 1) trees.head match {
      case _:Assign                         => // we don't want to include assignments
      case _:TermTree | _:Ident | _:Select  => // ... but do want these as valdefs.
        requestFromLine("val %s =\n%s".format(varName, line), synthetic) match {
          case Right(req) => return Right(req withOriginalLine line)
          case x          => return x
        }
      case _                                =>
    }
        
    // figure out what kind of request
    Right(buildRequest(line, lineNameCreator(), trees))
  }

  /** <p>
   *    Interpret one line of input.  All feedback, including parse errors
   *    and evaluation results, are printed via the supplied compiler's 
   *    reporter.  Values defined are available for future interpreted
   *    strings.
   *  </p>
   *  <p>
   *    The return value is whether the line was interpreter successfully,
   *    e.g. that there were no parse errors.
   *  </p>
   *
   *  @param line ...
   *  @return     ...
   */
  def interpret(line: String): IR.Result = interpret(line, false)
  def interpret(line: String, synthetic: Boolean): IR.Result = {
    def loadAndRunReq(req: Request) = {
      val (result, succeeded) = req.loadAndRun
      // don't truncate stack traces
      if (!succeeded) out print cleanNoTruncate(result)
      else if (printResults) out print clean(result)

      // book-keeping
      if (succeeded && !synthetic)
        recordRequest(req)
      
      if (succeeded) IR.Success
      else IR.Error
    }
    
    if (global == null) IR.Error
    else requestFromLine(line, synthetic) match {
      case Left(result) => result
      case Right(req)   => 
        // null indicates a disallowed statement type; otherwise compile and
        // fail if false (implying e.g. a type error)
        if (req == null || !req.compile) IR.Error
        else loadAndRunReq(req)
    }
  }

  /** A name creator used for objects created by <code>bind()</code>. */
  private lazy val newBinder = new NameCreator("binder")

  /** Bind a specified name to a specified value.  The name may
   *  later be used by expressions passed to interpret.
   *
   *  @param name      the variable name to bind
   *  @param boundType the type of the variable, as a string
   *  @param value     the object value to bind to it
   *  @return          an indication of whether the binding succeeded
   */
  def bind(name: String, boundType: String, value: Any): IR.Result = {
    val binderName = newBinder()

    compileString("""
      |object %s {
      |  var value: %s = _
      |  def set(x: Any) = value = x.asInstanceOf[%s]
      |}
    """.stripMargin.format(binderName, boundType, boundType))

    val binderObject = loadByName(binderName)
    val setterMethod = methodByName(binderObject, "set")
    
    setterMethod.invoke(null, value.asInstanceOf[AnyRef])
    interpret("val %s = %s.value".format(name, binderName))
  }

  def quietBind(p: NamedParam): IR.Result                  = beQuietDuring(bind(p))
  def bind(p: NamedParam): IR.Result                       = bind(p.name, p.tpe, p.value)
  def bind[T: Manifest](name: String, value: T): IR.Result = bind((name, value))

  /** Reset this interpreter, forgetting all user-specified requests. */
  def reset() {
    virtualDirectory.clear
    resetClassLoader()
    lineNameCreator.reset()
    varNameCreator.reset()
    prevRequests.clear
  }

  /** This instance is no longer needed, so release any resources
   *  it is using.  The reporter's output gets flushed.
   */
  def close() {
    reporter.flush
  }

  /** A traverser that finds all mentioned identifiers, i.e. things
   *  that need to be imported.  It might return extra names.
   */
  private class ImportVarsTraverser extends Traverser {
    val importVars = new mutable.HashSet[Name]()

    override def traverse(ast: Tree) = ast match {
      case Ident(name) =>
        // XXX this is obviously inadequate but it's going to require some effort
        // to get right.
        if (name.toString startsWith "x$") ()
        else importVars += name
      case _        => super.traverse(ast)
    }
  }

  /** Class to handle one member among all the members included
   *  in a single interpreter request.
   */
  private sealed abstract class MemberHandler(val member: Tree) {
    lazy val referencedNames: List[Name] = {
      val ivt = new ImportVarsTraverser()
      ivt traverse member
      ivt.importVars.toList
    }
    def boundNames: List[Name] = Nil
    val definesImplicit = cond(member) {
      case tree: MemberDef => tree.mods.isImplicit
    }    
    def generatesValue: Option[Name] = None

    def extraCodeToEvaluate(req: Request, code: PrintWriter) { }
    def resultExtractionCode(req: Request, code: PrintWriter) { }

    override def toString = "%s(used = %s)".format(this.getClass.toString split '.' last, referencedNames)
  }

  private class GenericHandler(member: Tree) extends MemberHandler(member)
  
  private class ValHandler(member: ValDef) extends MemberHandler(member) {
    val maxStringElements = 1000  // no need to mkString billions of elements
    lazy val ValDef(mods, vname, _, _) = member    
    lazy val prettyName = NameTransformer.decode(vname)
    
    override lazy val boundNames = List(vname)
    override def generatesValue = Some(vname)
    
    override def resultExtractionCode(req: Request, code: PrintWriter) {
      val isInternal = isGeneratedVarName(vname) && req.lookupTypeOf(vname) == "Unit"
      if (!mods.isPublic || isInternal) return
      
      lazy val extractor = "scala.runtime.ScalaRunTime.stringOf(%s, %s)".format(req fullPath vname, maxStringElements)
      
      // if this is a lazy val we avoid evaluating it here
      val resultString = if (mods.isLazy) codegenln(false, "<lazy>") else extractor
      val codeToPrint =
        """ + "%s: %s = " + %s""".format(prettyName, string2code(req typeOf vname), resultString)
      
      code print codeToPrint
    }
  }

  private class DefHandler(defDef: DefDef) extends MemberHandler(defDef) {
    lazy val DefDef(mods, name, _, vparamss, _, _) = defDef
    override lazy val boundNames = List(name)
    // true if 0-arity
    override def generatesValue =
      if (vparamss.isEmpty || vparamss.head.isEmpty) Some(name)
      else None

    override def resultExtractionCode(req: Request, code: PrintWriter) =
      if (mods.isPublic) code print codegenln(name, ": ", req.typeOf(name))
  }

  private class AssignHandler(member: Assign) extends MemberHandler(member) {
    val lhs = member.lhs.asInstanceOf[Ident] // an unfortunate limitation
    val helperName = newTermName(synthVarNameCreator())
    override def generatesValue = Some(helperName)

    override def extraCodeToEvaluate(req: Request, code: PrintWriter) =
      code println """val %s = %s""".format(helperName, lhs)

    /** Print out lhs instead of the generated varName */
    override def resultExtractionCode(req: Request, code: PrintWriter) {
      val lhsType = string2code(req lookupTypeOf helperName)
      val res = string2code(req fullPath helperName)
      val codeToPrint = """ + "%s: %s = " + %s + "\n" """.format(lhs, lhsType, res)
          
      code println codeToPrint
    }
  }

  private class ModuleHandler(module: ModuleDef) extends MemberHandler(module) {
    lazy val ModuleDef(mods, name, _) = module
    override lazy val boundNames = List(name)
    override def generatesValue = Some(name)

    override def resultExtractionCode(req: Request, code: PrintWriter) =
      code println codegenln("defined module ", name)
  }

  private class ClassHandler(classdef: ClassDef) extends MemberHandler(classdef) {
    lazy val ClassDef(mods, name, _, _) = classdef
    override lazy val boundNames = 
      name :: (if (mods.isCase) List(name.toTermName) else Nil)
    
    override def resultExtractionCode(req: Request, code: PrintWriter) =
      code print codegenln("defined %s %s".format(classdef.keyword, name))
  }

  private class TypeAliasHandler(typeDef: TypeDef) extends MemberHandler(typeDef) {
    lazy val TypeDef(mods, name, _, _) = typeDef
    def isAlias() = mods.isPublic && treeInfo.isAliasTypeDef(typeDef)
    override lazy val boundNames = if (isAlias) List(name) else Nil

    override def resultExtractionCode(req: Request, code: PrintWriter) =
      code println codegenln("defined type alias ", name)
  }

  private class ImportHandler(imp: Import) extends MemberHandler(imp) {
    val Import(expr, selectors) = imp
    def targetType = stringToCompilerType(expr.toString) match {
      case NoType => None
      case x      => Some(x)
    }
    
    private def selectorWild    = selectors filter (_.name == USCOREkw)   // wildcard imports, e.g. import foo._
    private def selectorRenames = selectors map (_.rename) filterNot (_ == null)
    
    /** Whether this import includes a wildcard import */
    val importsWildcard = selectorWild.nonEmpty
    
    /** Complete list of names imported by a wildcard */
    def wildcardImportedNames: List[Name] = (
      for (tpe <- targetType ; if importsWildcard) yield
        tpe.nonPrivateMembers filter (x => x.isMethod && x.isPublic) map (_.name) distinct
    ).toList.flatten

    /** The individual names imported by this statement */
    /** XXX come back to this and see what can be done with wildcards now that
     *  we know how to enumerate the identifiers.
     */
    val importedNames: List[Name] = 
      selectorRenames filterNot (_ == USCOREkw) flatMap (_.bothNames)
    
    override def resultExtractionCode(req: Request, code: PrintWriter) = {
      code println codegenln(imp.toString)
    }
  }

  /** One line of code submitted by the user for interpretation */
  private class Request(val line: String, val lineName: String, val trees: List[Tree]) {
    private var _originalLine: String = null
    def withOriginalLine(s: String): this.type = { _originalLine = s ; this }
    def originalLine = if (_originalLine == null) line else _originalLine
    
    /** name to use for the object that will compute "line" */
    def objectName = lineName + INTERPRETER_WRAPPER_SUFFIX

    /** name of the object that retrieves the result from the above object */
    def resultObjectName = RESULT_OBJECT_PREFIX + objectName

    /** handlers for each tree in this request */
    val handlers: List[MemberHandler] = trees map chooseHandler

    /** all (public) names defined by these statements */
    val boundNames = handlers flatMap (_.boundNames)

    /** list of names used by this expression */
    val referencedNames: List[Name] = handlers flatMap (_.referencedNames)
    
    /** def and val names */
    def defNames = partialFlatMap(handlers) { case x: DefHandler => x.boundNames }
    def valueNames = partialFlatMap(handlers) {
      case x: AssignHandler => List(x.helperName)
      case x: ValHandler    => boundNames
      case x: ModuleHandler => List(x.name)
    }
    /** Type names */
    def typeNames = handlers collect {
      case x: ClassHandler     => x.name
      case x: TypeAliasHandler => x.name
    }

    /** Code to import bound names from previous lines - accessPath is code to
      * append to objectName to access anything bound by request.
      */
    val ComputedImports(importsPreamble, importsTrailer, accessPath) =
      importsCode(Set.empty ++ referencedNames)

    /** Code to access a variable with the specified name */
    def fullPath(vname: String): String = "%s.`%s`".format(objectName + accessPath, vname)

    /** Code to access a variable with the specified name */
    def fullPath(vname: Name): String = fullPath(vname.toString)

    /** the line of code to compute */
    def toCompute = line

    /** generate the source code for the object that computes this request */
    def objectSourceCode: String = stringFromWriter { code => 
      val preamble = """
        |object %s {
        |  %s%s
      """.stripMargin.format(objectName, importsPreamble, indentCode(toCompute))     
      val postamble = importsTrailer + "\n}"

      code println preamble
      handlers foreach { _.extraCodeToEvaluate(this, code) }
      code println postamble
    }

    /** generate source code for the object that retrieves the result
        from objectSourceCode */
    def resultObjectSourceCode: String = stringFromWriter { code =>
      /** We only want to generate this code when the result
       *  is a value which can be referred to as-is.
       */      
      val valueExtractor = handlers.last.generatesValue match {
        case Some(vname) if typeOf contains vname =>
          """
          |lazy val scala_repl_value = {
          |  scala_repl_result
          |  %s
          |}""".stripMargin.format(fullPath(vname))
        case _  => ""
      }
      
      // first line evaluates object to make sure constructor is run
      // initial "" so later code can uniformly be: + etc
      val preamble = """
      |object %s {
      |  %s
      |  val scala_repl_result: String = {
      |    %s
      |    (""
      """.stripMargin.format(resultObjectName, valueExtractor, objectName + accessPath)
      
      val postamble = """
      |    )
      |  }
      |}
      """.stripMargin

      code println preamble
      handlers foreach { _.resultExtractionCode(this, code) }
      code println postamble
    }

    // compile the object containing the user's code
    lazy val objRun = compileAndSaveRun("<console>", objectSourceCode)

    // compile the result-extraction object
    lazy val extractionObjectRun = compileAndSaveRun("<console>", resultObjectSourceCode)

    lazy val loadedResultObject = loadByName(resultObjectName)
    
    def extractionValue(): Option[AnyRef] = {
      // ensure it has run
      extractionObjectRun
      
      // load it and retrieve the value        
      try Some(loadedResultObject getMethod "scala_repl_value" invoke loadedResultObject)
      catch { case _: Exception => None }
    }

    /** Compile the object file.  Returns whether the compilation succeeded.
     *  If all goes well, the "types" map is computed. */
    def compile(): Boolean = {
      // error counting is wrong, hence interpreter may overlook failure - so we reset
      reporter.reset

      // compile the main object
      objRun
      
      // bail on error
      if (reporter.hasErrors)
        return false

      // extract and remember types 
      typeOf
      definedTypes

      // compile the result-extraction object
      extractionObjectRun

      // success
      !reporter.hasErrors
    }

    def afterTyper[T](op: => T): T = atPhase(objRun.typerPhase.next)(op)

    /** The outermost wrapper object */
    lazy val outerResObjSym: Symbol = getMember(EmptyPackage, newTermName(objectName))

    /** The innermost object inside the wrapper, found by
      * following accessPath into the outer one. */
    lazy val resObjSym =
      accessPath.split("\\.").foldLeft(outerResObjSym) { (sym, name) =>
        if (name == "") sym else
        afterTyper(sym.info member newTermName(name))
      }

    /* typeOf lookup with encoding */
    def lookupTypeOf(name: Name) = typeOf.getOrElse(name, typeOf(global.encode(name)))
    def simpleNameOfType(name: TypeName) = (compilerTypeOf get name) map (_.typeSymbol.simpleName)
    
    private def typeMap[T](f: Type => T): Map[Name, T] = {
      def toType(name: Name): T = {
        // the types are all =>T; remove the =>
        val tp1 = afterTyper(resObjSym.info.nonPrivateDecl(name).tpe match {
          case NullaryMethodType(tp)  => tp
          case tp                 => tp
        })
        // normalize non-public types so we don't see protected aliases like Self
        afterTyper(tp1 match {
          case TypeRef(_, sym, _) if !sym.isPublic  => f(tp1.normalize)
          case tp                                   => f(tp)
        })
      }
      valueNames ++ defNames ++ typeNames map (x => x -> toType(x)) toMap
    }
    /** Types of variables defined by this request. */
    lazy val compilerTypeOf = typeMap[Type](x => x)
    /** String representations of same. */
    lazy val typeOf         = typeMap[String](_.toString)
    
    lazy val definedTypes: Map[Name, Type] = {
      typeNames map (x => x -> afterTyper(resObjSym.info.nonPrivateDecl(x).tpe)) toMap
    }
    
    private def bindExceptionally(t: Throwable) = {
      val ex: Exceptional =
        if (isettings.showInternalStackTraces) Exceptional(t)
        else new Exceptional(t) {
          override def spanFn(frame: JavaStackFrame) = !(frame.className startsWith resultObjectName)
          override def contextPrelude = super.contextPrelude + "/* The repl internal portion of the stack trace is elided. */\n"
        }
        
      quietBind("lastException", ex)
      ex.contextHead + "\n(access lastException for the full trace)"
    }
    private def bindUnexceptionally(t: Throwable) = {
      quietBind("lastException", t)
      stackTraceString(t)
    }

    /** load and run the code using reflection */
    def loadAndRun: (String, Boolean) = {
      import interpreter.Line._
      
      def handleException(t: Throwable) = {
        /** We turn off the binding to accomodate ticket #2817 */
        withoutBindingLastException {
          val message =
            if (opt.richExes) bindExceptionally(unwrap(t))
            else bindUnexceptionally(unwrap(t))
          
          (message, false)
        }
      }
      
      try {
        val resultValMethod = loadedResultObject getMethod "scala_repl_result"
        val execution       = lineManager.set(originalLine)(resultValMethod invoke loadedResultObject)

        execution.await()
        execution.state match {
          case Done       => ("" + execution.get(), true)
          case Threw      => if (bindLastException) handleException(execution.caught()) else throw execution.caught()
          case Cancelled  => ("Execution interrupted by signal.\n", false)
          case Running    => ("Execution still running! Seems impossible.", false)
        }
      }
      finally lineManager.clear()
    }

    override def toString = "Request(line=%s, %s trees)".format(line, trees.size)
  }

  /** Returns the name of the most recent interpreter result.
   *  Mostly this exists so you can conveniently invoke methods on
   *  the previous result.
   */
  def mostRecentVar: String =
    if (mostRecentlyHandledTree.isEmpty) ""
    else mostRecentlyHandledTree.get match {
      case x: ValOrDefDef           => x.name
      case Assign(Ident(name), _)   => name
      case ModuleDef(_, name, _)    => name
      case _                        => varNameCreator.mostRecent
    }
  
  private def requestForName(name: Name): Option[Request] =
    prevRequests.reverse find (_.boundNames contains name)

  private def requestForIdent(line: String): Option[Request] = requestForName(newTermName(line))
  
  // XXX literals.
  def stringToCompilerType(id: String): Type = {
    // if it's a recognized identifier, the type of that; otherwise treat the
    // String like a value (e.g. scala.collection.Map) .
    def findType = typeForIdent(id) match {
      case Some(x)  => definitions.getClass(newTermName(x)).tpe
      case _        => definitions.getModule(newTermName(id)).tpe
    }

    try findType catch { case _: MissingRequirementError => NoType }
  }
    
  def typeForIdent(id: String): Option[String] =
    requestForIdent(id) flatMap (x => x.typeOf get newTermName(id))

  def methodsOf(name: String) =
    evalExpr[List[String]](methodsCode(name)) map (x => NameTransformer.decode(getOriginalName(x)))
  
  def completionAware(name: String) = {
    // XXX working around "object is not a value" crash, i.e.
    // import java.util.ArrayList ; ArrayList.<tab>
    clazzForIdent(name) flatMap (_ => evalExpr[Option[CompletionAware]](asCompletionAwareCode(name)))
  }
    
  def extractionValueForIdent(id: String): Option[AnyRef] =
    requestForIdent(id) flatMap (_.extractionValue)
  
  /** Executes code looking for a manifest of type T.
   */
  def manifestFor[T: Manifest] =
    evalExpr[Manifest[T]]("""manifest[%s]""".format(manifest[T]))

  /** Executes code looking for an implicit value of type T.
   */
  def implicitFor[T: Manifest] = {
    val s = manifest[T].toString
    evalExpr[Option[T]]("{ def f(implicit x: %s = null): %s = x ; Option(f) }".format(s, s))
    // We don't use implicitly so as to fail without failing.
    // evalExpr[T]("""implicitly[%s]""".format(manifest[T]))
  }

  def clazzForIdent(id: String): Option[Class[_]] =
    extractionValueForIdent(id) flatMap (x => Option(x) map (_.getClass))

  private def methodsCode(name: String) =
    "%s.%s(%s)".format(classOf[ReflectionCompletion].getName, "methodsOf", name)
  
  private def asCompletionAwareCode(name: String) =
    "%s.%s(%s)".format(classOf[CompletionAware].getName, "unapply", name)

  private def getOriginalName(name: String): String =
    nme.originalName(newTermName(name)).toString

  case class InterpreterEvalException(msg: String) extends Exception(msg)
  def evalError(msg: String) = throw InterpreterEvalException(msg)
  
  /** The user-facing eval in :power mode wraps an Option.
   */
  def eval[T: Manifest](line: String): Option[T] =
    try Some(evalExpr[T](line))
    catch { case InterpreterEvalException(msg) => out println indentCode(msg) ; None }

  def evalExpr[T: Manifest](line: String): T = {
    // Nothing means the type could not be inferred.
    if (manifest[T] eq Manifest.Nothing)
      evalError("Could not infer type: try 'eval[SomeType](%s)' instead".format(line))
    
    val lhs = getSynthVarName
    beQuietDuring { interpret("val " + lhs + " = { " + line + " } ") }
    
    // TODO - can we meaningfully compare the inferred type T with
    //   the internal compiler Type assigned to lhs?
    // def assignedType = prevRequests.last.typeOf(newTermName(lhs))

    val req = requestFromLine(lhs, true) match {
      case Left(result) => evalError(result.toString)
      case Right(req)   => req
    }
    if (req == null || !req.compile || req.handlers.size != 1)
      evalError("Eval error.")
      
    try req.extractionValue.get.asInstanceOf[T] catch {
      case e: Exception => evalError(e.getMessage)
    }
  }
  
  def interpretExpr[T: Manifest](code: String): Option[T] = beQuietDuring {
    interpret(code) match {
      case IR.Success =>
        try prevRequests.last.extractionValue map (_.asInstanceOf[T])
        catch { case e: Exception => out println e ; None }
      case _ => None
    }
  }

  /** Another entry point for tab-completion, ids in scope */
  private def unqualifiedIdNames() = partialFlatMap(allHandlers) {
    case x: AssignHandler => List(x.helperName)
    case x: ValHandler    => List(x.vname)
    case x: ModuleHandler => List(x.name)
    case x: DefHandler    => List(x.name)
    case x: ImportHandler => x.importedNames
  } filterNot isSynthVarName
  
  /** Types which have been wildcard imported, such as:
   *    val x = "abc" ; import x._  // type java.lang.String
   *    import java.lang.String._   // object java.lang.String
   *
   *  Used by tab completion.
   *
   *  XXX right now this gets import x._ and import java.lang.String._,
   *  but doesn't figure out import String._.  There's a lot of ad hoc
   *  scope twiddling which should be swept away in favor of digging
   *  into the compiler scopes.
   */
  def wildcardImportedTypes(): List[Type] = {
    val xs = allHandlers collect { case x: ImportHandler if x.importsWildcard => x.targetType }
    xs.flatten.reverse.distinct
  }
  
  /** Another entry point for tab-completion, ids in scope */
  def unqualifiedIds() = (unqualifiedIdNames() map (_.toString)).distinct.sorted

  /** For static/object method completion */ 
  def getClassObject(path: String): Option[Class[_]] = classLoader tryToLoadClass path
  
  /** Parse the ScalaSig to find type aliases */
  def aliasForType(path: String) = ByteCode.aliasForType(path)
  
  // debugging
  def isCompletionDebug = settings.Ycompletion.value
  def DBG(s: => String) =
    try if (isReplDebug) repldbg(s)
    catch { case x: AssertionError => repldbg("Assertion error printing debug string:\n  " + x) }
}

/** Utility methods for the Interpreter. */
object IMain {
  import scala.collection.generic.CanBuildFrom
  def partialFlatMap[A, B, CC[X] <: Traversable[X]]
    (coll: CC[A])
    (pf: PartialFunction[A, CC[B]])
    (implicit bf: CanBuildFrom[CC[A], B, CC[B]]) =
  {
    val b = bf(coll)
    for (x <- coll collect pf)
      b ++= x

    b.result
  }

  def codegenln(leadingPlus: Boolean, xs: String*): String = codegen(leadingPlus, (xs ++ Array("\n")): _*)
  def codegenln(xs: String*): String = codegenln(true, xs: _*)

  def codegen(xs: String*): String = codegen(true, xs: _*)
  def codegen(leadingPlus: Boolean, xs: String*): String = {
    val front = if (leadingPlus) "+ " else ""
    front + (xs map string2codeQuoted mkString " + ")
  }
  
  def string2codeQuoted(str: String) = "\"" + string2code(str) + "\""

  /** Convert a string into code that can recreate the string.
   *  This requires replacing all special characters by escape
   *  codes. It does not add the surrounding " marks.  */
  def string2code(str: String): String = {    
    val res = new StringBuilder
    for (c <- str) c match {
      case '"' | '\'' | '\\'  => res += '\\' ; res += c
      case _ if c.isControl   => res ++= Chars.char2uescape(c)
      case _                  => res += c
    }
    res.toString
  }
}
