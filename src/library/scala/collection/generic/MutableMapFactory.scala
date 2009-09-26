/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package generic

import mutable.MapBuilder

/** A template for companion objects of mutable.Map and subclasses thereof.
 *
 *  @since 2.8
 */
abstract class MutableMapFactory[CC[A, B] <: mutable.Map[A, B] with mutable.MapLike[A, B, CC[A, B]]] 
  extends MapFactory[CC] {

  def newBuilder[A, B] = new MapBuilder[A, B, CC[A, B]](empty[A, B])
}
