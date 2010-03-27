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
trait Integral[@specialized(Int) T] extends Numeric[T] {
  def quot(x: T, y: T): T
  def rem(x: T, y: T): T
  
  override implicit def mkNumericOps(lhs1: T): IntegralOps[T] = new IntegralOps[T] {
    val lhs = lhs1
    val n = Integral.this
  }
}

trait IntegralOps[@specialized(Int) T] extends NumericOps[T] {
  override val n: Integral[T]
  import n._

  def /(rhs: T) = quot(lhs, rhs)
  def %(rhs: T) = rem(lhs, rhs)
  def /%(rhs: T) = (quot(lhs, rhs), rem(lhs, rhs))
}
  
