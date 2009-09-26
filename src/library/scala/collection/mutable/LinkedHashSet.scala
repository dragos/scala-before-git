/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package mutable

import generic._

/** Todo: this has O(n) cost for element removal.
 *  Should be rewritten to be more efficient.
 *  @since 2.2
 */
@serializable
class LinkedHashSet[A] extends Set[A] 
                          with GenericSetTemplate[A, LinkedHashSet]
                          with SetLike[A, LinkedHashSet[A]] 
                          with FlatHashTable[A] 
{
  override def companion: GenericCompanion[LinkedHashSet] = LinkedHashSet

  protected val ordered = new ListBuffer[A]

  override def size = super.size

  def contains(elem: A): Boolean = containsEntry(elem)

  def += (elem: A): this.type = { add(elem); this }
  def -= (elem: A): this.type = { remove(elem); this }

  override def add(elem: A): Boolean =
    if (addEntry(elem)) { ordered += elem; true }
    else false
  
  override def remove(elem: A): Boolean = 
    removeEntry(elem) match {
      case None => false
      case _ => ordered -= elem; true
    }
  
  override def clear() {
    ordered.clear()
    super.clear()
  }

  override def iterator = ordered.iterator

  override def foreach[U](f: A => U) = ordered foreach f
}

/** Factory object for `LinkedHashSet` class */
object LinkedHashSet extends SetFactory[LinkedHashSet] {
  implicit def builderFactory[A]: BuilderFactory[A, LinkedHashSet[A], Coll] = setBuilderFactory[A]
  override def empty[A]: LinkedHashSet[A] = new LinkedHashSet[A]
}

