/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend
package icode

/** This trait ...
 *
 *  @author  Iulian Dragos
 *  @version 1.0
 */
trait TypeStacks { self: ICodes =>
  import opcodes._
  import global.{Symbol, Type, definitions}

  class TypeStack[T] {
    var types: List[T] = Nil

    def this(stack: List[T]) = {
      this()
      this.types = stack
    }

    def this(that: TypeStack[T]) = this(that.types)

    def length: Int = types.length

    /** Push a type on the type stack. UNITs are ignored. */
    def push(t: T): this.type = {
      if (t != UNIT)
        types = t :: types
      this
    }

    def pushAll(ts: Traversable[T]): this.type = {
      ts foreach push
      this
    }

    def head: T = types.head

    /** Removes the value on top of the stack, and returns it. It assumes
     *  the stack contains at least one element.
     */
    def pop: T = {
      val t = types.head
      types = types.tail
      t
    }

    def clear {
      types = Nil
    }

    /** Return the topmost two values on the stack. It assumes the stack
     *  is large enough. Topmost element first.
     */
    def pop2: (T, T) = (pop, pop)

    /** Return the topmost three values on the stack. It assumes the stack
     *  is large enough. Topmost element first.
     */
    def pop3: (T, T, T) = (pop, pop, pop)

    /** Drop the first n elements of the stack. */
    def pop(n: Int): List[T] = {
      val prefix = types.take(n)
      types = types.drop(n)
      prefix
    }
    
    def apply(n: Int): T = types(n)

    /** In-place map f over elements of this stack. */
    def map(f: T => T): this.type = {
      types = types map f
      this
    }

    /**
     * A TypeStack agrees with another one if they have the same
     * length and each type kind agrees position-wise. Two 
     * types agree if one is a subtype of the other.
     */
    def agreesWith(other: TypeStack[T])(implicit lt: (T, T) => Boolean) : Boolean =
      (types.length == other.types.length) &&
      ((types, other.types).zipped forall ((t1, t2) => lt(t1, t2) || lt(t2, t1)))

    /* This method returns a String representation of the stack */
    override def toString() = types.mkString("[", ", ", "]")

    override def hashCode() = types.hashCode()
    override def equals(other: Any): Boolean = other match {
      case x: TypeStack[_] => x.types sameElements types
      case _            => false
    }
  }

}
