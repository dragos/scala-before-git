/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala

/** Annotate type parameters on which code should be automatically
 *  specialized. For example:
 *  {{{
 *    class MyList[@specialized T] ...
 *  }}}
 *
 *  Type T can be specialized on a subset of the primitive types by
 *  specifying a number of types separated by |:
 *
 *  {{{
 *   class MyList[@specialized[Int|Double|Boolean] T] ..
 *  }}}
 *
 *  @since 2.8
 */
class specialized[T <: AnyVal] extends StaticAnnotation

