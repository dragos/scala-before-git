/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */
 
package scala.tools
package util

import java.lang.reflect.{ Method, Constructor }
import scala.tools.reflect._
import scala.collection.{ mutable, immutable }
import nsc.io.timer
import nsc.util.{ ScalaClassLoader, Exceptional }
import Exceptional.unwrap
import scala.util.Random

/** Signal handling code.  100% clean of any references to sun.misc:
 *  it's all reflection and proxies and invocation handlers and lasers,
 *  so even the choosiest runtimes will be cool with it.
 *  
 *  Sun/Oracle says sun.misc.* is unsupported and therefore so is all
 *  of this.  Simple examples:
 *  {{{
      val manager = scala.tools.util.SignalManager // or you could make your own
      // Assignment clears any old handlers; += chains them.
      manager("HUP") = println("HUP 1!")
      manager("HUP") += println("HUP 2!")
      // Use raise() to raise a signal: this will print both lines 
      manager("HUP").raise()
      // See a report on every signal's current handler
      manager.dump()
 *  }}}
 */
class SignalManager(classLoader: ScalaClassLoader) {
  def this() = this(ScalaClassLoader.getSystemLoader)
  private val illegalArgHandler: PartialFunction[Throwable, Boolean] = {
    case x if unwrap(x).isInstanceOf[IllegalArgumentException] => false
  }
  private def fail(msg: String) = new SignalError(msg)
  
  object rSignalHandler extends Shield {
    val className   = "sun.misc.SignalHandler"
    val classLoader = SignalManager.this.classLoader
    
    lazy val SIG_DFL = field("SIG_DFL") get null
    lazy val SIG_IGN = field("SIG_IGN") get null
    
    /** Create a new signal handler based on the function.
     */
    def apply(action: Invoked => Unit) = Mock.fromInterfaces(clazz) {
      case inv @ Invoked.NameAndArgs("handle", _ :: Nil) => action(inv)
    }
    def empty = rSignalHandler(_ => ())
  }
  import rSignalHandler.{ SIG_DFL, SIG_IGN }

  object rSignal extends Shield {
    val className   = "sun.misc.Signal"
    val classLoader = SignalManager.this.classLoader
    
    lazy val handleMethod = method("handle", 2)
    lazy val raiseMethod  = method("raise", 1)
    lazy val numberMethod = method("getNumber", 0)
    
    /** Create a new Signal with the given name.
     */
    def apply(name: String)                     = constructor(classOf[String]) newInstance name
    def handle(signal: AnyRef, current: AnyRef) = {
      if (signal == null || current == null) fail("Signals cannot be null")
      else handleMethod.invoke(null, signal, current)
    }
    def raise(signal: AnyRef)                   = {
      if (signal == null) fail("Signals cannot be null")
      else raiseMethod.invoke(null, signal)
    }
    def number(signal: AnyRef): Int             = numberMethod.invoke(signal).asInstanceOf[Int]
    
    class WSignal(val name: String) {
      lazy val signal             = rSignal apply name
      def number                  = rSignal number signal
      def raise()                 = rSignal raise signal
      def handle(handler: AnyRef) = rSignal.handle(signal, handler)
      
      def setTo(body: => Unit)  = register(name, false, body)
      def +=(body: => Unit)     = register(name, true, body)

      /** It's hard to believe there's no way to get a signal's current
       *  handler without replacing it, but if there is I couldn't find
       *  it, so we have this swapping code.
       */
      def withCurrentHandler[T](f: AnyRef => T): T = {
        val swap = handle(rSignalHandler.empty)

        try f(swap)
        finally handle(swap)
      }
      def isDefault = try withCurrentHandler {
        case SIG_DFL  => true
        case _        => false
      } catch illegalArgHandler
      def isIgnored = try withCurrentHandler {
        case SIG_IGN  => true
        case _        => false
      } catch illegalArgHandler
      def isSetTo(ref: AnyRef) =
        try withCurrentHandler { _ eq ref }
        catch illegalArgHandler

      def handlerString() = withCurrentHandler {
        case SIG_DFL    => "Default"
        case SIG_IGN    => "Ignore"
        case x          => "" + x
      }

      override def toString = "%10s  %s".format("SIG" + name,
        try handlerString()
        catch { case x: Exception => "VM threw " + unwrap(x) }
      )
      override def equals(other: Any) = other match {
        case x: WSignal => name == x.name
        case _          => false
      }
      override def hashCode = name.##
    }
  }
  type WSignal = rSignal.WSignal

  /** Adds a handler for the named signal.  If shouldChain is true,
   *  the installed handler will call the previous handler after the
   *  new one has executed.  If false, the old handler is dropped.
   */
  private def register(name: String, shouldChain: Boolean, body: => Unit) = {
    val signal  = rSignal(name)
    val current = rSignalHandler(_ => body)
    val prev    = rSignal.handle(signal, current)
    
    if (shouldChain) {
      val chainer = rSignalHandler { inv =>
        val signal = inv.args.head

        inv invokeOn current
        prev match {
          case SIG_IGN | SIG_DFL  => ()
          case _                  => inv invokeOn prev
        }
      }
      rSignal.handle(signal, chainer)
      chainer
    }
    else current
  }
  
  /** Use apply and update to get and set handlers.
   */
  def apply(name: String): WSignal =
    try   { new WSignal(name) }
    catch { case x: IllegalArgumentException => new SignalError(x.getMessage) }
  
  def update(name: String, body: => Unit): Unit = apply(name) setTo body

  class SignalError(message: String) extends WSignal(null) {
    override def toString = message
  }
  
  def public(name: String, description: String)(body: => Unit): Unit = {
    val wsig = apply(name)
    wsig setTo body
    registerInfoHandler()
    addPublicHandler((wsig, description))
  }
  /** Makes sure the info handler is registered if we see activity. */
  private def registerInfoHandler() = {
    val INFO = apply("INFO")
    if (publicHandlers.isEmpty && INFO.isDefault) {
      INFO setTo Console.println(info())
      addPublicHandler((INFO, "Dump list of well known signal handler to console."))
    }
  }
  private def addPublicHandler(kv: (WSignal, String)) = {
    if (publicHandlers exists (_._1 == kv._1)) ()
    else publicHandlers = (kv :: publicHandlers) sortBy (_._1.number)
  }
  private var publicHandlers: List[(WSignal, String)] = Nil
  def info(): String = {
    registerInfoHandler()
    "\nOutward facing signal handler registry:\n" + (
      publicHandlers map { case (wsig, descr) => "  %2d  %5s  %s\n".format(wsig.number, wsig.name, descr) } mkString ""
    )
  }
}

object SignalManager extends SignalManager {
  private implicit def mkWSignal(name: String): WSignal = this(name)
  private lazy val signalNumberMap = all map (x => x.number -> x) toMap
  
  def all = List(
    HUP, INT, QUIT, ILL, TRAP, ABRT, EMT, FPE,    // 1-8
    KILL, BUS, SEGV, SYS, PIPE, ALRM, TERM, URG,  // 9-15
    STOP, TSTP, CONT, CHLD, TTIN, TTOU, IO, XCPU, // 16-23
    XFSZ, VTALRM, PROF, WINCH, INFO, USR1, USR2   // 24-31
  )
  /** Signals which seem like particularly bad choices
   *  when looking for an open one.
   */
  def reserved = Set(QUIT, TRAP, ABRT, KILL, BUS, SEGV, ALRM, STOP, INT)
  def unreserved = all filterNot reserved
  
  def defaultSignals() = unreserved filter (_.isDefault)
  def ignoredSignals() = unreserved filter (_.isIgnored)
  def findOpenSignal() = Random.shuffle(defaultSignals()).head
  
  def dump() = all foreach (x => println("%2s %s".format(x.number, x)))

  def apply(sigNumber: Int): WSignal = signalNumberMap(sigNumber)

  def HUP: WSignal    = "HUP"
  def INT: WSignal    = "INT"
  def QUIT: WSignal   = "QUIT"
  def ILL: WSignal    = "ILL"
  def TRAP: WSignal   = "TRAP"
  def ABRT: WSignal   = "ABRT"
  def EMT: WSignal    = "EMT"
  def FPE: WSignal    = "FPE"
  def KILL: WSignal   = "KILL"
  def BUS: WSignal    = "BUS"
  def SEGV: WSignal   = "SEGV"
  def SYS: WSignal    = "SYS"
  def PIPE: WSignal   = "PIPE"
  def ALRM: WSignal   = "ALRM"
  def TERM: WSignal   = "TERM"
  def URG: WSignal    = "URG"
  def STOP: WSignal   = "STOP"
  def TSTP: WSignal   = "TSTP"
  def CONT: WSignal   = "CONT"
  def CHLD: WSignal   = "CHLD"
  def TTIN: WSignal   = "TTIN"
  def TTOU: WSignal   = "TTOU"
  def IO: WSignal     = "IO"
  def XCPU: WSignal   = "XCPU"
  def XFSZ: WSignal   = "XFSZ"
  def VTALRM: WSignal = "VTALRM"
  def PROF: WSignal   = "PROF"
  def WINCH: WSignal  = "WINCH"
  def INFO: WSignal   = "INFO"
  def USR1: WSignal   = "USR1"
  def USR2: WSignal   = "USR2"
  
  /** Given a number of seconds, a signal, and a function: sets up a handler which upon
   *  receiving the signal once, calls the function with argument true, and if the
   *  signal is received again within the allowed time, calls it with argument false.
   *  (Otherwise it calls it with true and starts the timer over again.)
   */
  def requireInterval(seconds: Int, wrapper: WSignal)(fn: Boolean => Unit) = {
    var received = false
    wrapper setTo {
      if (received) fn(false)
      else {
        received = true
        fn(true)
        timer(seconds)(received = false)
      }
    }
  }  
}
