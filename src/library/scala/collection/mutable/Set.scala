/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import generic._

/** A generic trait for mutable sets. Concrete set implementations
 *  have to provide functionality for the abstract methods in Set:
 *
 *  def contains(elem: A): Boolean
 *  def iterator: Iterator[A]
 *  def += (elem: A): this.type
 *  def -= (elem: A): this.type
 *
 *  @author Matthias Zenger
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   1
 */
trait Set[A] extends Iterable[A]
                with scala.collection.Set[A]
                with GenericSetTemplate[A, Set]
                with SetLike[A, Set[A]] {
  override def companion: GenericCompanion[Set] = Set
}
                
/** The canonical factory methods for <a href="Set.html">mutable sets</a>.
 *  Currently this returns a HashSet.
 */
object Set extends SetFactory[Set] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] = setCanBuildFrom[A]
  override def empty[A]: Set[A] = HashSet.empty[A]
}

