/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable


/** This class is typically used as a mixin. It turns maps which map <code>A</code>
 *  to <code>Set[B]</code> objects into multi maps which map <code>A</code> to
 *  <code>B</code> objects.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky 
 *  @version 2.8
 *  @since   1
 */
trait MultiMap[A, B] extends Map[A, Set[B]] {
  protected def makeSet: Set[B] = new HashSet[B]

  @deprecated("use addBinding instead")
  def add(key: A, value: B): this.type = addBinding(key, value)

  def addBinding(key: A, value: B): this.type = {
    get(key) match {
      case None =>
        val set = makeSet
        set += value
        this(key) = set
      case Some(set) =>
        set += value
    }
    this
  }

  def removeBinding(key: A, value: B): this.type = {
    get(key) match {
      case None =>
        case Some(set) =>
          set -= value
          if (set.isEmpty) this -= key
    }
    this
  }

  def entryExists(key: A, p: B => Boolean): Boolean = get(key) match {
    case None => false
    case Some(set) => set exists p
  }
}
