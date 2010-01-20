/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
import generic._

import mutable.ListBuffer
import immutable.List
import scala.util.control.Breaks._

/** A template trait for linear sequences of type `LinearSeq[A]`.
 *
 *  $linearSeqInfo
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 *  @since   2.8
 *
 *  @define  Coll  LinearSeq
 *  @define  linearSeqInfo
 *  Linear sequences are defined in terms of three abstract methods, which are assumed
 *  to have efficient implementations. These are:
 *  {{{
 *     def isEmpty: Boolean
 *     def head: A
 *     def tail: Repr
 *  }}}
 *  Here, `A` is the type of the sequence elements and `Repr` is the type of the sequence itself.
 *
 *  Linear sequences do not define any new methods wrt `Seq`. However, abstract `Seq` methods
 *  are defined in terms of `isEmpty`, `head`, and `tail`, and several other methods are overridden
 *  with optimized implementations.
 *
 *  @tparam A    the element type of the $coll
 *  @tparam Repr the type of the actual $coll containing the elements.
 */
trait LinearSeqLike[+A, +Repr <: LinearSeqLike[A, Repr]] extends SeqLike[A, Repr] { self: Repr =>

  override protected[this] def thisCollection: LinearSeq[A] = this.asInstanceOf[LinearSeq[A]]
  override protected[this] def toCollection(repr: Repr): LinearSeq[A] = repr.asInstanceOf[LinearSeq[A]]

  def isEmpty: Boolean

  def head: A

  def tail: Repr

  /** The length of the $coll.
   * 
   *  $willNotTerminateInf
   *
   *  Note: the execution of `length` may take time proportial to the length of the sequence.
   */
  def length: Int = {
    var these = self
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  /** Selects an element by its index in the $coll.
   *  Note: the execution of `apply` may take time proportial to the index value.
   *  @throws `IndexOutOfBoundsEsxception` if `idx` does not satisfy `0 <= idx < length`.
   */
  def apply(n: Int): A = {
    val rest = drop(n)
    if (n < 0 || rest.isEmpty) throw new IndexOutOfBoundsException
    rest.head
  }

  override /*IterableLike*/ 
  def iterator: Iterator[A] = new Iterator[A] {
    var these = self
    def hasNext: Boolean = !these.isEmpty
    def next: A = 
      if (hasNext) {
        val result = these.head; these = these.tail; result
      } else Iterator.empty.next
    override def toList: List[A] = these.toList
  }

  override /*IterableLike*/ 
  def foreach[B](f: A => B) {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

                                                                                   
  override /*IterableLike*/ 
  def forall(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  override /*IterableLike*/ 
  def exists(p: A => Boolean): Boolean = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  override /*TraversableLike*/ 
  def count(p: A => Boolean): Int = {
    var these = this
    var cnt = 0
    while (!these.isEmpty) {
      if (p(these.head)) cnt += 1
      these = these.tail
    }
    cnt
  }

  override /*IterableLike*/ 
  def find(p: A => Boolean): Option[A] = {
    var these = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }
/*
  override def mapFind[B](f: A => Option[B]): Option[B] = {
    var res: Option[B] = None
    var these = this
    while (res.isEmpty && !these.isEmpty) {
      res = f(these.head)
      these = these.tail
    }
    res
  }
*/
  override /*TraversableLike*/ 
  def foldLeft[B](z: B)(f: (B, A) => B): B = {
    var acc = z
    var these = this
    while (!these.isEmpty) {
      acc = f(acc, these.head)
      these = these.tail
    }
    acc
  }

  override /*IterableLike*/ 
  def foldRight[B](z: B)(f: (A, B) => B): B = 
    if (this.isEmpty) z
    else f(head, tail.foldRight(z)(f))

  override /*TraversableLike*/ 
  def reduceLeft[B >: A](f: (B, A) => B): B = 
    if (isEmpty) throw new UnsupportedOperationException("empty.reduceLeft")
    else tail.foldLeft[B](head)(f)

  override /*IterableLike*/ 
  def reduceRight[B >: A](op: (A, B) => B): B = 
    if (isEmpty) throw new UnsupportedOperationException("Nil.reduceRight")
    else if (tail.isEmpty) head
    else op(head, tail.reduceRight(op))
  
  override /*TraversableLike*/ 
  def last: A = {
    if (isEmpty) throw new NoSuchElementException
    var these = this
    var nx = these.tail
    while (!nx.isEmpty) {
      these = nx
      nx = nx.tail
    }
    these.head
  }
  
  override /*IterableLike*/ 
  def take(n: Int): Repr = {
    val b = newBuilder
    var i = 0
    var these = repr
    while (!these.isEmpty && i < n) {
      i += 1
      b += these.head
      these = these.tail
    }
    b.result
  }
  
  override /*TraversableLike*/ 
  def drop(n: Int): Repr = {
    var these: Repr = repr
    var count = n
    while (!these.isEmpty && count > 0) {
      these = these.tail
      count -= 1
    }
    these
  }

  override /*IterableLike*/ 
  def dropRight(n: Int): Repr = {
    val b = newBuilder
    var these = this
    var lead = this drop n
    while (!lead.isEmpty) {
      b += these.head
      these = these.tail
      lead = lead.tail
    }
    b.result
  }

  override /*IterableLike*/ 
  def slice(from: Int, until: Int): Repr = {
    val b = newBuilder 
    var i = from
    var these = this drop from
    while (i < until && !these.isEmpty) {
      b += these.head
      these = these.tail
      i += 1
    }
    b.result
  }

  override /*IterableLike*/ 
  def takeWhile(p: A => Boolean): Repr = {
    val b = newBuilder
    var these = this
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    b.result
  }

  override /*TraversableLike*/ 
  def span(p: A => Boolean): (Repr, Repr) = {
    var these: Repr = repr
    val b = newBuilder
    while (!these.isEmpty && p(these.head)) {
      b += these.head
      these = these.tail
    }
    (b.result, these)
  }  

  override /*IterableLike*/ 
  def sameElements[B >: A](that: Iterable[B]): Boolean = that match {
    case that1: LinearSeq[_] =>
      var these = this
      var those = that1
      while (!these.isEmpty && !those.isEmpty && these.head == those.head) {
        these = these.tail
        those = those.tail
      }
      these.isEmpty && those.isEmpty
    case _ =>
      super.sameElements(that)
  }
  
  override /*SeqLike*/
  def lengthCompare(len: Int): Int =  {
    var i = 0
    var these = self
    while (!these.isEmpty && i <= len) {
      i += 1
      these = these.tail
    }
    i - len
  }

  override /*SeqLike*/
  def isDefinedAt(x: Int): Boolean = x >= 0 && lengthCompare(x) > 0

  override /*SeqLike*/
  def segmentLength(p: A => Boolean, from: Int): Int = {
    var i = 0
    var these = this drop from
    while (!these.isEmpty && p(these.head)) {
      i += 1
      these = these.tail
    }
    i
  }

  override /*SeqLike*/
  def indexWhere(p: A => Boolean, from: Int): Int = {
    var i = from
    var these = this drop from
    while (these.nonEmpty) {
      if (p(these.head))
        return i
      
      i += 1
      these = these.tail
    }
    -1
  }

  override /*SeqLike*/
  def lastIndexWhere(p: A => Boolean, end: Int): Int = {
    var i = 0
    var these = this
    var last = -1
    while (!these.isEmpty && i <= end) {
      if (p(these.head)) last = i
      these = these.tail
      i += 1
    }
    last
  }
}
