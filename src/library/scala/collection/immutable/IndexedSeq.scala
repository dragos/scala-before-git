/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package immutable

import generic._
import mutable.{ArrayBuffer, Builder}

/** A subtrait of <code>collection.IndexedSeq</code> which represents indexed sequences
 *  that cannot be mutated.
 *  $indexedSeqInfo
 *
 *  @since 2.8
 */
trait IndexedSeq[+A] extends Seq[A] 
                    with scala.collection.IndexedSeq[A] 
                    with GenericTraversableTemplate[A, IndexedSeq]
                    with IndexedSeqLike[A, IndexedSeq[A]] { 
  override def companion: GenericCompanion[IndexedSeq] = IndexedSeq
}

/**
 * @since 2.8
 */
object IndexedSeq extends SeqFactory[IndexedSeq] {
  @serializable
  class Impl[A](buf: ArrayBuffer[A]) extends IndexedSeq[A] {
    def length = buf.length
    def apply(idx: Int) = buf.apply(idx)
  }
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, IndexedSeq[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, IndexedSeq[A]] = Vector.newBuilder[A]
}
