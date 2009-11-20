/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import generic._

/** <p>
 *    A generic trait for immutable sets. Concrete set implementations have
 *    to provide functionality for the abstract methods in <code>Set</code>:
 *  </p>
 *  <pre>
 *    <b>def</b> contains(elem: A): Boolean
 *    <b>def</b> iterator: Iterator[A]
 *    <b>def</b> + (elem: A): Self
 *    <b>def</b> - (elem: A): Self</pre>
 *  <p>
 *    where <code>Self</code> is the type of the set.
 *  </p>
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

object Set extends SetFactory[Set] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Set[A]] = setCanBuildFrom[A]
  override def empty[A]: Set[A] = new EmptySet[A]

  private val hashSeed = "Set".hashCode 

  /** An optimized representation for immutable empty sets */
  @serializable
  class EmptySet[A] extends Set[A] {
    override def size: Int = 0
    def contains(elem: A): Boolean = false
    def + (elem: A): Set[A] = new Set1(elem)
    def - (elem: A): Set[A] = this
    def iterator: Iterator[A] = Iterator.empty
    override def foreach[U](f: A =>  U): Unit = {}
  }
  
  /** An optimized representation for immutable sets of size 1 */
  @serializable @SerialVersionUID(1233385750652442003L)
  class Set1[A](elem1: A) extends Set[A] {
    override def size: Int = 1
    def contains(elem: A): Boolean = 
      elem == elem1
    def + (elem: A): Set[A] = 
      if (contains(elem)) this
      else new Set2(elem1, elem)
    def - (elem: A): Set[A] = 
      if (elem == elem1) new EmptySet[A] 
      else this
    def iterator: Iterator[A] = 
      Iterator(elem1)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1)
    }
  }

  /** An optimized representation for immutable sets of size 2 */
  @serializable @SerialVersionUID(-6443011234944830092L)
  class Set2[A](elem1: A, elem2: A) extends Set[A] {
    override def size: Int = 2
    def contains(elem: A): Boolean = 
      elem == elem1 || elem == elem2
    def + (elem: A): Set[A] = 
      if (contains(elem)) this
      else new Set3(elem1, elem2, elem)
    def - (elem: A): Set[A] = 
      if (elem == elem1) new Set1(elem2)
      else if (elem == elem2) new Set1(elem1)
      else this
    def iterator: Iterator[A] = 
      Iterator(elem1, elem2)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1); f(elem2)
    }
  }

  /** An optimized representation for immutable sets of size 3 */
  @serializable @SerialVersionUID(-3590273538119220064L)
  class Set3[A](elem1: A, elem2: A, elem3: A) extends Set[A] {
    override def size: Int = 3
    def contains(elem: A): Boolean = 
      elem == elem1 || elem == elem2 || elem == elem3
    def + (elem: A): Set[A] = 
      if (contains(elem)) this
      else new Set4(elem1, elem2, elem3, elem)
    def - (elem: A): Set[A] = 
      if (elem == elem1) new Set2(elem2, elem3)
      else if (elem == elem2) new Set2(elem1, elem3)
      else if (elem == elem3) new Set2(elem1, elem2)
      else this
    def iterator: Iterator[A] = 
      Iterator(elem1, elem2, elem3)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1); f(elem2); f(elem3)
    }
  }

  /** An optimized representation for immutable sets of size 4 */
  @serializable @SerialVersionUID(-3622399588156184395L)
  class Set4[A](elem1: A, elem2: A, elem3: A, elem4: A) extends Set[A] {
    override def size: Int = 4
    def contains(elem: A): Boolean = 
      elem == elem1 || elem == elem2 || elem == elem3 || elem == elem4
    def + (elem: A): Set[A] = 
      if (contains(elem)) this
      else new HashSet[A] + (elem1, elem2, elem3, elem4, elem)
    def - (elem: A): Set[A] = 
      if (elem == elem1) new Set3(elem2, elem3, elem4)
      else if (elem == elem2) new Set3(elem1, elem3, elem4)
      else if (elem == elem3) new Set3(elem1, elem2, elem4)
      else if (elem == elem4) new Set3(elem1, elem2, elem3)
      else this
    def iterator: Iterator[A] = 
      Iterator(elem1, elem2, elem3, elem4)
    override def foreach[U](f: A =>  U): Unit = {
      f(elem1); f(elem2); f(elem3); f(elem4)
    }
  }
}

