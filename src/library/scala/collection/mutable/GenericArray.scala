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

/** This class is used internally to implement data structures that
 *  are based on resizable arrays.
 *
 *  @author  Matthias Zenger, Burak Emir
 *  @author Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
class GenericArray[A](override val length: Int)
extends IndexedSeq[A] 
   with GenericTraversableTemplate[A, GenericArray]
   with IndexedSeqLike[A, GenericArray[A]] { 

  override def companion: GenericCompanion[GenericArray] = GenericArray

  val array: Array[AnyRef] = new Array[AnyRef](length)

  def apply(idx: Int): A = {
    if (idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    array(idx).asInstanceOf[A]
  }

  def update(idx: Int, elem: A) { 
    if (idx >= length) throw new IndexOutOfBoundsException(idx.toString)
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  /** Fills the given array <code>xs</code> with the elements of
   *  this sequence starting at position <code>start</code>.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   */
  override def copyToArray[B >: A](xs: Array[B], start: Int) {
    Array.copy(array, 0, xs, start, length)
  }

  /** Copy all elements to a buffer 
   *  @param   The buffer to which elements are copied
  override def copyToBuffer[B >: A](dest: Buffer[B]) {
    dest ++= (array: Seq[AnyRef]).asInstanceOf[Seq[B]]
  }
   */

  override def foreach[U](f: A =>  U) {
    var i = 0
    while (i < length) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }
}

object GenericArray extends SeqFactory[GenericArray] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, GenericArray[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, GenericArray[A]] = 
    new ArrayBuffer[A] mapResult { buf => 
      val result = new GenericArray[A](buf.length)
      buf.copyToArray(result.array.asInstanceOf[Array[Any]], 0)
      result
    }
}
