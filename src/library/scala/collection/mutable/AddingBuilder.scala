/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package mutable

import generic._

/** The canonical builder for collections that are addable, i.e. that support an efficient + method
 *  which adds an element to the collection.
 *  Collections are built from their empty element using this + method.
 *  @param empty   The empty element of the collection.
 */
class AddingBuilder[A, Coll <: Addable[A, Coll] with scala.collection.Iterable[A] with scala.collection.IterableLike[A, Coll]](empty: Coll) 
extends Builder[A, Coll] {
  protected var elems: Coll = empty
  def +=(x: A): this.type = { elems = elems + x; this }
  def clear() { elems = empty }
  def result: Coll = elems
}
