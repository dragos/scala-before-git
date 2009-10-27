/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package mutable

import scala.reflect.ClassManifest

/**
 * @since 2.8
 */
abstract class ArrayOps[T] extends ArrayLike[T, Array[T]] {

  private def rowBuilder[U]: Builder[U, Array[U]] = 
    Array.newBuilder(
      ClassManifest.fromClass(
        repr.getClass.getComponentType.getComponentType.asInstanceOf[Predef.Class[U]]))

  /** Flattens a two-dimensional array by concatenating all its rows
   *  into a single array
   */
  def flatten[U](implicit asArray: T => /*<:<!!!*/ Array[U]): Array[U] = {
    val b = rowBuilder[U]
    for (xs <- this)
      b ++= asArray(xs)
    b.result
  }

  /** Transposes a two dimensional array
   */
  def transpose[U](implicit asArray: T => Array[U]): Array[Array[U]] = {
    val bs = asArray(head) map (_ => rowBuilder[U])
    for (xs <- this) {
      var i = 0
      for (x <- asArray(xs)) {
        bs(i) += x
        i += 1
      }
    }
    val bb: Builder[Array[U], Array[Array[U]]] = Array.newBuilder(
      ClassManifest.fromClass(
        repr.getClass.getComponentType.asInstanceOf[Predef.Class[Array[U]]]))
    for (b <- bs) bb += b.result
    bb.result
  }

  /** Covariant override of Object.clone. It cannot be abstract because it would be overriden by
   *  the inherited clone, which has weaker access privileges.  
   */
  override def clone: Array[T] = error("This method needs to be concrete")
}

/**
 * @since 2.8
 */
object ArrayOps {

  class ofRef[T <: AnyRef](override val repr: Array[T]) extends ArrayOps[T] with ArrayLike[T, Array[T]] {

    override protected[this] def thisCollection: WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def toCollection(repr: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(
      ClassManifest.classType[T](repr.getClass.getComponentType))

    def length: Int = repr.length
    def apply(index: Int): T = repr(index)
    def update(index: Int, elem: T) { repr(index) = elem }
    override def clone: Array[T] = runtime.ArrayRuntime.cloneArray(repr.asInstanceOf[Array[AnyRef]]).asInstanceOf[Array[T]]
  }

  class ofByte(override val repr: Array[Byte]) extends ArrayOps[Byte] with ArrayLike[Byte, Array[Byte]] {

    override protected[this] def thisCollection: WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def toCollection(repr: Array[Byte]): WrappedArray[Byte] = new WrappedArray.ofByte(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofByte

    def length: Int = repr.length
    def apply(index: Int): Byte = repr(index)
    def update(index: Int, elem: Byte) { repr(index) = elem }
    override def clone: Array[Byte] = runtime.ArrayRuntime.cloneArray(repr)
  }

  class ofShort(override val repr: Array[Short]) extends ArrayOps[Short] with ArrayLike[Short, Array[Short]] {

    override protected[this] def thisCollection: WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def toCollection(repr: Array[Short]): WrappedArray[Short] = new WrappedArray.ofShort(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofShort

    def length: Int = repr.length
    def apply(index: Int): Short = repr(index)
    def update(index: Int, elem: Short) { repr(index) = elem }
    override def clone: Array[Short] = runtime.ArrayRuntime.cloneArray(repr)
  }

  class ofChar(override val repr: Array[Char]) extends ArrayOps[Char] with ArrayLike[Char, Array[Char]] {

    override protected[this] def thisCollection: WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def toCollection(repr: Array[Char]): WrappedArray[Char] = new WrappedArray.ofChar(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofChar

    def length: Int = repr.length
    def apply(index: Int): Char = repr(index)
    def update(index: Int, elem: Char) { repr(index) = elem }
    override def clone: Array[Char] = runtime.ArrayRuntime.cloneArray(repr)
  } 

  class ofInt(override val repr: Array[Int]) extends ArrayOps[Int] with ArrayLike[Int, Array[Int]] {

    override protected[this] def thisCollection: WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def toCollection(repr: Array[Int]): WrappedArray[Int] = new WrappedArray.ofInt(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofInt

    def length: Int = repr.length
    def apply(index: Int): Int = repr(index)
    def update(index: Int, elem: Int) { repr(index) = elem }
    override def clone: Array[Int] = runtime.ArrayRuntime.cloneArray(repr)
  } 

  class ofLong(override val repr: Array[Long]) extends ArrayOps[Long] with ArrayLike[Long, Array[Long]] {

    override protected[this] def thisCollection: WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def toCollection(repr: Array[Long]): WrappedArray[Long] = new WrappedArray.ofLong(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofLong

    def length: Int = repr.length
    def apply(index: Int): Long = repr(index)
    def update(index: Int, elem: Long) { repr(index) = elem }
    override def clone: Array[Long] = runtime.ArrayRuntime.cloneArray(repr)
  } 

  class ofFloat(override val repr: Array[Float]) extends ArrayOps[Float] with ArrayLike[Float, Array[Float]] {

    override protected[this] def thisCollection: WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def toCollection(repr: Array[Float]): WrappedArray[Float] = new WrappedArray.ofFloat(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofFloat

    def length: Int = repr.length
    def apply(index: Int): Float = repr(index)
    def update(index: Int, elem: Float) { repr(index) = elem }
    override def clone: Array[Float] = runtime.ArrayRuntime.cloneArray(repr)
  } 

  class ofDouble(override val repr: Array[Double]) extends ArrayOps[Double] with ArrayLike[Double, Array[Double]] {

    override protected[this] def thisCollection: WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def toCollection(repr: Array[Double]): WrappedArray[Double] = new WrappedArray.ofDouble(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofDouble

    def length: Int = repr.length
    def apply(index: Int): Double = repr(index)
    def update(index: Int, elem: Double) { repr(index) = elem }
    override def clone: Array[Double] = runtime.ArrayRuntime.cloneArray(repr)
  } 

  class ofBoolean(override val repr: Array[Boolean]) extends ArrayOps[Boolean] with ArrayLike[Boolean, Array[Boolean]] {

    override protected[this] def thisCollection: WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def toCollection(repr: Array[Boolean]): WrappedArray[Boolean] = new WrappedArray.ofBoolean(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofBoolean

    def length: Int = repr.length
    def apply(index: Int): Boolean = repr(index)
    def update(index: Int, elem: Boolean) { repr(index) = elem }
    override def clone: Array[Boolean] = runtime.ArrayRuntime.cloneArray(repr)
  } 

  class ofUnit(override val repr: Array[Unit]) extends ArrayOps[Unit] with ArrayLike[Unit, Array[Unit]] {

    override protected[this] def thisCollection: WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def toCollection(repr: Array[Unit]): WrappedArray[Unit] = new WrappedArray.ofUnit(repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofUnit

    def length: Int = repr.length
    def apply(index: Int): Unit = repr(index)
    def update(index: Int, elem: Unit) { repr(index) = elem }
    override def clone: Array[Unit] = runtime.ArrayRuntime.cloneArray(repr.asInstanceOf[Array[AnyRef]]).asInstanceOf[Array[Unit]]
  } 
}
