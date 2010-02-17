/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.math

/**
 * @since 2.8
 */
trait Fractional[T] extends Numeric[T] {
  def div(x: T, y: T): T
  
  class FractionalOps(lhs1: T) extends NumericOps[T] {
    val lhs = lhs1
    val n = Fractional.this

    def /(rhs: T) = div(lhs, rhs)
  }
  override implicit def mkNumericOps(lhs: T): FractionalOps =
    new FractionalOps(lhs)
}
