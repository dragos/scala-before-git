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

import collection.mutable.Buffer

// Methods could be printed by  cat IterableTemplate.scala | egrep '^  (override )?def'


/** This trait implements a proxy for iterable objects. It forwards
 *  all calls to a different iterable object
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait IterableProxyTemplate[+A, +This <: IterableTemplate[A, This] with Iterable[A]]
    extends IterableTemplate[A, This]
    with TraversableProxyTemplate[A, This]
{   
  override def iterator: Iterator[A] = self.iterator
  override def foreach[U](f: A => U): Unit = self.foreach(f)
  override def isEmpty: Boolean = self.isEmpty
  override def foldRight[B](z: B)(op: (A, B) => B): B = self.foldRight(z)(op)
  override def reduceRight[B >: A](op: (A, B) => B): B = self.reduceRight(op)
  override def toIterable: Iterable[A] = self.toIterable
  override def zip[A1 >: A, B, That](that: Iterable[B])(implicit bf: BuilderFactory[(A1, B), That, This]): That = self.zip[A1, B, That](that)(bf)
  override def zipAll[B, A1 >: A, That](that: Iterable[B], thisElem: A1, thatElem: B)(implicit bf: BuilderFactory[(A1, B), That, This]): That = self.zipAll(that, thisElem, thatElem)(bf)
  override def zipWithIndex[A1 >: A, That](implicit bf: BuilderFactory[(A1, Int), That, This]): That = self.zipWithIndex(bf)
  override def head: A = self.head
  override def takeRight(n: Int): This = self.takeRight(n)
  override def dropRight(n: Int): This = self.dropRight(n)
  override def sameElements[B >: A](that: Iterable[B]): Boolean = self.sameElements(that)
  override def toStream: Stream[A] = self.toStream
  override def view = self.view
  override def view(from: Int, until: Int) = self.view(from, until)
}
