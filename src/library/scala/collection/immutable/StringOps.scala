/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package immutable

import mutable.StringBuilder

/**
 * @since 2.8
 */
final class StringOps(override val repr: String) extends StringLike[String] {

  override protected[this] def thisCollection: WrappedString = new WrappedString(repr)
  override protected[this] def toCollection(repr: String): WrappedString = new WrappedString(repr)

  /** Creates a string builder buffer as builder for this class */
  override protected[this] def newBuilder = new StringBuilder
  
  override def slice(from: Int, until: Int): String = 
    repr.substring(from max 0, until min repr.length)  

  override def toString = repr
}  
