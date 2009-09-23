/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.runtime

//import java.lang.Character
import collection.Vector
import collection.generic.VectorView
import Predef.{require, NoSuchElementException}

/** <p>
 *    For example, in the following code
 *  </p>
 *  <pre>
 *    <b>object</b> test <b>extends</b> Application {
 *      Console.println(<chr>'\40'</chr>.isWhitespace)
 *      Console.println('\011'.isWhitespace)
 *      Console.println('1'.asDigit == 1)
 *      Console.println('A'.asDigit == 10)
 *    }</pre>
 *  <p>
 *    the implicit conversions are performed using the predefined view
 *    <a href="../Predef$object.html#charWrapper(scala.Char)"
 *    target="contentFrame"><code>Predef.charWrapper</code></a>.
 *  </p>
 */
final class RichChar(x: Char) extends Proxy with Ordered[Char] {

  // Proxy.self
  def self: Any = x

  // Ordered[Char].compare
  def compare (y: Char): Int = if (x < y) -1 else if (x > y) 1 else 0

  def asDigit: Int = System.Char.GetNumericValue(x).toInt

  def isControl: Boolean = System.Char.IsControl(x)
  def isDigit: Boolean = System.Char.IsDigit(x)
  def isLetter: Boolean = System.Char.IsLetter(x)
  def isLetterOrDigit: Boolean = System.Char.IsLetterOrDigit(x)
  def isLowerCase: Boolean = System.Char.IsLower(x)
  def isUpperCase: Boolean = System.Char.IsUpper(x)
  def isWhitespace: Boolean = System.Char.IsWhiteSpace(x)

  def toLowerCase: Char = System.Char.ToLower(x)
  def toUpperCase: Char = System.Char.ToUpper(x)
  
  /** Create a <code>[Char]</code> over the characters from 'x' to 'y' - 1
   */
  def until(limit: Char): VectorView[Char, Vector[Char]] =
    if (limit <= x) Vector.empty.view
    else
      new VectorView[Char, Vector[Char]] {
        protected def underlying = Vector.empty[Char]
        def length = limit - x
        def apply(i: Int): Char = {
          require(i >= 0 && i < length)
          (x + i).toChar
        }
      }

  /** Create a <code>VectorView[Char]</code> over the characters from 'x' to 'y'
   */
  def to(y: Char): VectorView[Char, Vector[Char]] = until((y + 1).toChar)

}
