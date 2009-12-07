/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.util.control

/** A class that can be instantiated for the break control abstraction.
 *  Example usage:<pre>
 *
 *  val mybreaks = new Breaks
 *  import</b> mybreaks.{break, breakable}
 *
 *  breakable {
 *    <b>for</b> (...) {
 *      <b>if</b> (...) break
 *    }
 *  }</pre>
 *
 *  Calls to break from one instantiation of Breaks will never
 *  target breakable objects of some other instantion.
 */  
class Breaks {

  private val breakException = new BreakException

  /** A block from which one can exit with a `break''. */
  def breakable(op: => Unit) {
    try {
      op
    } catch {
      case ex: BreakException => 
        if (ex ne breakException) throw ex
    }
  }

  /* Break from dynamically closest enclosing breakable block
   * @note this might be different than the statically closest enclosing
   * block!
   */
  def break { throw breakException }
} 

/** An object that can be used for the break control abstraction.
 *  Example usage:<pre>
 *
 *  <b>import</b> Breaks.{break, breakable}
 *
 *  breakable {
 *    <b>for</b> (...) {
 *      <b>if</b> (...) break
 *    }
 *  }</pre>
 *
 */  
object Breaks extends Breaks

private class BreakException extends RuntimeException with ControlException

