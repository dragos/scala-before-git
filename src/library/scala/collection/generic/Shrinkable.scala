/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.generic
import scala.collection._

/** This class represents collections that can be reduced using a -= operator.
 *        
 *  @author   Martin Odersky
 *  @owner   Martin Odersky
 *  @version 2.8
 */
trait Shrinkable[-A] { 

  /** Removes a single element from this collection.
   *
   *  @param elem  the element to remove.
   */
  def -=(elem: A): this.type

  /** Removes two or more elements from this collection.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   */
  def -=(elem1: A, elem2: A, elems: A*): this.type = {
    this -= elem1 
    this -= elem2
    this --= elems
  }

  /** Removes a number of elements provided by an iterator from this collection.
   *
   *  @param iter  the iterator.
   */
  def --=(iter: Iterator[A]): this.type = { iter foreach -=; this }

  /** Removes a number of elements provided by an iterable object from this collection.
   *
   *  @param iter  the iterable object.
   */
  def --=(iter: Traversable[A]): this.type = { iter foreach -=; this }
}
  

  

