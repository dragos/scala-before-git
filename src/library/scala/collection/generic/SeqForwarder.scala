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
import scala.collection.immutable.Range

/** This class implements a forwarder for sequences. It forwards
 *  all calls to a different sequence object except for
 *
 *    - toString, hashCode, equals, stringPrefix
 *    - newBuilder, view, toSeq
 *    - all calls creating a new sequence of the same kind
 *
 *  The above methods are forwarded by subclass SeqProxy
 *
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait SeqForwarder[+A] extends Seq[A] with IterableForwarder[A] {

  protected override def underlying: Seq[A] 
  
  // PartialFunction delegates

  override def apply(i: Int): A = underlying.apply(i)
  override def isDefinedAt(x: Int): Boolean = underlying.isDefinedAt(x)
  
  // Seq delegates
  // Seq methods could be printed by  cat SeqLike.scala | sed -n '/trait Seq/,$ p' | egrep '^  (override )?def'
  
  override def length: Int = underlying.length
  override def lengthCompare(l: Int) = underlying lengthCompare l
  override def segmentLength(p: A => Boolean, from: Int): Int = underlying.segmentLength(p, from)
  override def prefixLength(p: A => Boolean) = underlying.prefixLength(p)
  override def indexWhere(p: A => Boolean, from: Int): Int = underlying.indexWhere(p, from)
  override def indexOf[B >: A](elem: B, from: Int): Int = underlying.indexOf(elem, from)
  override def reverseIterator: Iterator[A] = underlying.reverseIterator
  override def startsWith[B](that: Seq[B], offset: Int): Boolean = underlying.startsWith(that, offset)
  override def endsWith[B](that: Seq[B]): Boolean = underlying.endsWith(that)
  override def indexOfSlice[B >: A](that: Seq[B]): Int = underlying.indexOfSlice(that)
  override def contains(elem: Any): Boolean = underlying.contains(elem)
  override def indices: Range = underlying.indices
}
