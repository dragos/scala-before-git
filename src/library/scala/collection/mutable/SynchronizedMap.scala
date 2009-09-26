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


/** This class should be used as a mixin. It synchronizes the <code>Map</code>
 *  functions of the class into which it is mixed in.
 *
 *  @author  Matthias Zenger, Martin Odersky
 *  @version 2.0, 31/12/2006
 *  @since   1
 */
trait SynchronizedMap[A, B] extends Map[A, B] {

  abstract override def get(key: A): Option[B] = synchronized { super.get(key) }
  abstract override def iterator: Iterator[(A, B)] = synchronized { super.iterator }
  abstract override def += (kv: (A, B)): this.type = synchronized[this.type] { super.+=(kv) }
  abstract override def -= (key: A): this.type = synchronized[this.type] { super.-=(key) }

  override def size: Int = synchronized { super.size }
  override def put(key: A, value: B): Option[B] = synchronized { super.put(key, value) }
  override def update(key: A, value: B): Unit = synchronized { super.update(key, value) }
  override def remove(key: A): Option[B] = synchronized { super.remove(key) }
  override def clear(): Unit = synchronized { super.clear() }
  override def getOrElseUpdate(key: A, default: => B): B = synchronized { super.getOrElseUpdate(key, default) }
  override def transform(f: (A, B) => B): this.type = synchronized[this.type] { super.transform(f) }
  override def retain(p: (A, B) => Boolean): this.type = synchronized[this.type] { super.retain(p) }
  override def valueIterable: scala.collection.Iterable[B] = synchronized { super.valueIterable }
  @deprecated("Use `valuesIterator' instead") override def values: Iterator[B] = synchronized { super.valuesIterator }
  override def valuesIterator: Iterator[B] = synchronized { super.valuesIterator }
  override def clone() = synchronized { super.clone() }
  override def foreach[U](f: ((A, B)) => U) = synchronized { super.foreach(f) }
  override def apply(key: A): B = synchronized { super.apply(key) }
  override def keySet: scala.collection.Set[A] = synchronized { super.keySet }
  @deprecated("Use `keysIterator' instead") override def keys: Iterator[A] = synchronized { super.keysIterator }
  override def keysIterator: Iterator[A] = synchronized { super.keysIterator }
  override def isEmpty: Boolean = synchronized { super.isEmpty }
  override def contains(key: A): Boolean = synchronized {super.contains(key) }
  override def isDefinedAt(key: A) = synchronized { super.isDefinedAt(key) }

  @deprecated("See Map.+ for explanation") override def +(kv: (A, B)): this.type = synchronized[this.type] { super.+(kv) }
  // can't override -, -- same type!
  // @deprecated override def -(key: A): This = synchronized { super.-(key) }

  // !!! todo: also add all other methods 
}

