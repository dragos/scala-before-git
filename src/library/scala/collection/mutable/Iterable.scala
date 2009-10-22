/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.collection
package mutable

import generic._

/** <p>
 *    A subtrait of <a href="../Iterable.html" target="contentFrame">
 *    <code>collection.Iterable</code></a> which represents iterables
 *    that can be mutated.
 *  </p>
 *    
 *  @author   Martin Odersky
 *  @version 2.8
 *  @since   2.8
 */
trait Iterable[A] extends Traversable[A] 
                     with scala.collection.Iterable[A] 
                     with GenericTraversableTemplate[A, Iterable]
                     with IterableLike[A, Iterable[A]] { 
  override def companion: GenericCompanion[Iterable] = Iterable
}	

/** <p>
 *    A factory object for the trait <a href="Iterable.html"
 *    target="contentFrame"><code>Iterable</code></a>.
 *  </p>
 *    
 *  @author   Martin Odersky
 *  @version 2.8
 */
object Iterable extends TraversableFactory[Iterable] {
  implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, Iterable[A]] = new GenericCanBuildFrom[A]
  def newBuilder[A]: Builder[A, Iterable[A]] = new ArrayBuffer
}

