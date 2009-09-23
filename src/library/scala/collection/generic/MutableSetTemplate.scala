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

import script._

/** <p>
 *    A generic template for mutable sets of elements of type <code>A</code>.
 *    To implement a concrete mutable set, you need to provide implementations
 *    of the following methods:
 *  </p><pre>
 *    <b>def</b> contains(elem: A): Boolean
 *    <b>def</b> iterator: Iterator[A]
 *    <b>def</b> += (elem: A): <b>this.type</b>
 *    <b>def</b> -= (elem: A): <b>this.type</b></pre>
 *  <p>
 *    If you wish that methods <code>like</code>, <code>take</code>,
 *    <code>drop</code>, <code>filter</code> return the same kind of map,
 *    you should also override:
 *  </p><pre>
 *    <b>def</b> empty: This</pre>
 *  <p>
 *    It is also good idea to override methods <code>foreach</code> and
 *    <code>size</code> for efficiency.
 *  </p>
 * 
 */
trait MutableSetTemplate[A, +This <: MutableSetTemplate[A, This] with mutable.Set[A]] 
  extends SetTemplate[A, This]
     with Scriptable[A]
     with Builder[A, This]
     with Growable[A]
     with Shrinkable[A] 
     with Cloneable[mutable.Set[A]] 
{ self =>
  
  /** A common implementation of <code>newBuilder</code> for all mutable sets
   *  in terms of <code>empty</code>. Overrides <code>SetTemplate</code>
   *  implementation for better efficiency.
   */
  override protected[this] def newBuilder: Builder[A, This] = empty

  /** Adds a new element to the set.
   *
   *  @param elem the element to be added
   *  @return true if the element was not yet present in the set.
   */
  def add(elem: A): Boolean = {
    val r = contains(elem)
    this += elem
    r
  }

  /** Removes a single element from a set.
   *
   *  @param elem  The element to be removed.
   *  @return  true if the element was already present in the set.
   */
  def remove(elem: A): Boolean = {
    val r = contains(elem)
    this -= elem
    r
  }

  /** This method allows one to add or remove an element <code>elem</code>
   *  from this set depending on the value of parameter <code>included</code>.
   *  Typically, one would use the following syntax:
   *  <pre>set(elem) = true</pre>
   *
   */
  def update(elem: A, included: Boolean) {
    if (included) this += elem else this -= elem
  }

  /** Adds a new element to the set.
   *
   *  @param elem the element to be added
   */
  def +=(elem: A): this.type

  /** Removes a single element from a set. 
   *  @param elem  The element to be removed.
   */
  def -=(elem: A): this.type

  /** Removes all elements from the set for which the predicate <code>p</code>
   *  yields the value <code>false</code>.
   */
  def retain(p: A => Boolean): Unit = for (elem <- this.toList) if (!p(elem)) this -= elem

  /** Removes all elements from the set. After this operation is completed,
   *  the set will be empty.
   */
  def clear() { foreach(-=) }

  override def clone(): mutable.Set[A] = empty ++= repr

  def result: This = repr

  /** Adds a single element to this collection and returns 
   *  the collection itself.
   *
   *  @param elem  the element to add.
   */
  @deprecated("Use += instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() +=' if you intend to create a new collection.")
 override def + (elem: A): This = { +=(elem); repr }

  /** Adds two or more elements to this collection and returns
   *  the collection itself.
   *
   *  @param elem1 the first element to add.
   *  @param elem2 the second element to add.
   *  @param elems the remaining elements to add.
   */
  @deprecated("Use += instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() +=' if you intend to create a new collection.")
  override def + (elem1: A, elem2: A, elems: A*): This = {
    this += elem1 += elem2 ++= elems
    repr
  }

  /** Adds a number of elements provided by a traversable object and returns
   *  either the collection itself.
   *
   *  @param iter     the iterable object.
   */
  @deprecated("Use ++= instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() ++=' if you intend to create a new collection.")
  override def ++(iter: Traversable[A]): This = { 
    for (elem <- iter) +=(elem)
    repr
  }


  /** Adds a number of elements provided by an iterator and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   */
  @deprecated("Use ++= instead if you intend to add by side effect to an existing collection.\n"+
              "Use `clone() ++=' if you intend to create a new collection.")
  override def ++ (iter: Iterator[A]): This = {
    for (elem <- iter) +=(elem)
    repr
  }

  /** Removes a single element from this collection and returns 
   *  the collection itself.
   *
   *  @param elem  the element to remove.
   */
  @deprecated("Use -= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() -=' if you intend to create a new collection.")
  override def -(elem: A): This = { -=(elem); repr }

  /** Removes two or more elements from this collection and returns
   *  the collection itself.
   *
   *  @param elem1 the first element to remove.
   *  @param elem2 the second element to remove.
   *  @param elems the remaining elements to remove.
   */
  @deprecated("Use -= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() -=' if you intend to create a new collection.")
  override def -(elem1: A, elem2: A, elems: A*): This = {
    this -= elem1 -= elem2 --= elems
    repr
  }

  /** Removes a number of elements provided by a Traversable object and returns
   *  the collection itself.
   *
   *  @param iter     the Traversable object.
   */
  @deprecated("Use --= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() --=' if you intend to create a new collection.")
  override def --(iter: Traversable[A]): This = { 
    for (elem <- iter) -=(elem)
    repr
  }

  /** Removes a number of elements provided by an iterator and returns
   *  the collection itself.
   *
   *  @param iter   the iterator
   */
  @deprecated("Use --= instead if you intend to remove by side effect from an existing collection.\n"+
              "Use `clone() --=' if you intend to create a new collection.")
  override def --(iter: Iterator[A]): This = { 
    for (elem <- iter) -=(elem)
    repr
  }

  /** Send a message to this scriptable object.
   *
   *  @param cmd  the message to send.
   *  @throws <code>Predef.UnsupportedOperationException</code>
   *  if the message was not understood.
   */
   def <<(cmd: Message[A]): Unit = cmd match {
     case Include(_, x)     => this += x
     case Remove(_, x)      => this -= x
     case Reset()           => clear
     case s: Script[_]      => s.iterator foreach <<
     case _                 => throw new UnsupportedOperationException("message " + cmd + " not understood")
   }
}
