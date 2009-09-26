/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package generic

import scala.collection._
import mutable.{Builder, AddingBuilder}

/**
 * @since 2.8
 */
trait BitSetFactory[Coll <: BitSet with BitSetLike[Coll]] { 
  def newBuilder: Builder[Int, Coll] = new AddingBuilder[Int, Coll](empty)
  def empty: Coll
  def apply(elems: Int*): Coll = (empty /: elems) (_ + _)
  def bitsetBuilderFactory = new BuilderFactory[Int, Coll, Coll] {
    def apply(from: Coll) = newBuilder
  }
}

