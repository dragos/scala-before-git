/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package ast

import java.io.{PrintWriter, StringWriter}

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.symtab.{Flags, SymbolTable}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.util.{FreshNameCreator, HashSet, Position, NoPosition, SourceFile}


trait Trees { 
  self: SymbolTable =>

  //statistics

  var nodeCount = 0

  trait CompilationUnitTrait {
    var body: Tree
    val source: SourceFile
    def fresh : FreshNameCreator
  }

  type CompilationUnit <: CompilationUnitTrait

  // sub-components --------------------------------------------------

  object treePrinters extends {
    val trees: Trees.this.type = Trees.this
  } with TreePrinters

  lazy val treePrinter = 
    if (settings.Ycompacttrees.value) treePrinters.createCompact()
    else treePrinters.create()

  object treeInfo extends {
    val trees: Trees.this.type = Trees.this
  } with TreeInfo

  val treeCopy = new LazyTreeCopier()

  // modifiers --------------------------------------------------------

  /** @param privateWithin the qualifier for a private (a type name)
   *    or nme.EMPTY.toTypeName, if none is given.
   *  @param annotations the annotations for the definition.
   *    <strong>Note:</strong> the typechecker drops these annotations,
   *    use the AnnotationInfo's (Symbol.annotations) in later phases. 
   */  
  case class Modifiers(flags: Long, privateWithin: Name, annotations: List[Tree], positions: Map[Long, Position]) {
    def isCovariant     = hasFlag(COVARIANT    )  // marked with `+'
    def isContravariant = hasFlag(CONTRAVARIANT)  // marked with `-'
    def isPrivate   = hasFlag(PRIVATE  )
    def isProtected = hasFlag(PROTECTED)
    def isVariable  = hasFlag(MUTABLE  )
    def isArgument  = hasFlag(PARAM    )
    def isAccessor  = hasFlag(ACCESSOR )
    def isOverride  = hasFlag(OVERRIDE )
    def isAbstract  = hasFlag(ABSTRACT )
    def isDeferred  = hasFlag(DEFERRED )
    def isCase      = hasFlag(CASE     )
    def isLazy      = hasFlag(LAZY     )
    def isSealed    = hasFlag(SEALED   )
    def isFinal     = hasFlag(FINAL    )
    def isTrait     = hasFlag(TRAIT | notDEFERRED) // (part of DEVIRTUALIZE)
    def isImplicit  = hasFlag(IMPLICIT )
    def isPublic    = !isPrivate && !isProtected
    def hasFlag(flag: Long) = (flag & flags) != 0L
    def & (flag: Long): Modifiers = {
      val flags1 = flags & flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations, positions)
    }
    def &~ (flag: Long): Modifiers = {
      val flags1 = flags & (~flag)
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations, positions)
    }
    def | (flag: Long): Modifiers = {
      val flags1 = flags | flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations, positions)
    }
    def withAnnotations(annots: List[Tree]) =
      if (annots.isEmpty) this
      else Modifiers(flags, privateWithin, annotations ::: annots, positions)
    def withPosition(flag: Long, position: Position) =
    	Modifiers(flags, privateWithin, annotations, positions + (flag -> position))
  }

  def Modifiers(flags: Long, privateWithin: Name): Modifiers = Modifiers(flags, privateWithin, List(), new Map.EmptyMap)
  def Modifiers(flags: Long): Modifiers = Modifiers(flags, nme.EMPTY.toTypeName)

  val NoMods = Modifiers(0)

  // @M helper method for asserts that check consistency in kinding
  //def kindingIrrelevant(tp: Type) = (tp eq null) || phase.name == "erasure" || phase.erasedTypes 
  
  abstract class Tree extends Product {
    if (util.Statistics.enabled) {
      util.Statistics.nodeByType(getClass) += 1
    }

    val id = nodeCount
//    assert(id != 1223)
    nodeCount += 1

    private var rawpos: Position = NoPosition

    def pos = rawpos

    private[this] var rawtpe: Type = _

    def tpe = rawtpe
    def tpe_=(t: Type) = rawtpe = t

    def setPos(pos: Position): this.type = { 
      rawpos = pos
/*
      for (c <- this.children) 
        if (c.pos.isOpaqueRange && !pos.includes(c.pos)) {
          assert(false, "non-enclosing positions in "+this)
        }
*/
      this
    }
    
    /** Set tpe to give `tp` and return this.
     */
    def setType(tp: Type): this.type = { 
      /*assert(kindingIrrelevant(tp) || !kindStar || !tp.isHigherKinded, 
               tp+" should not be higher-kinded"); */ 
      tpe = tp
      this 
    }

    /** Like `setType`, but if this is a previously empty TypeTree
     *  that fact is remembered so that resetType will snap back.
     */
    def defineType(tp: Type): this.type = setType(tp)

    /** Reset type to `null`, with special handling of TypeTrees and the EmptyType
     */
    def resetType() { tpe = null }

    def symbol: Symbol = null
    def symbol_=(sym: Symbol) {
      throw new Error("symbol_= inapplicable for " + this)
    }
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }

    def hasSymbol = false
    def isDef = false
    def isTerm = false
    def isType = false
    def isEmpty = false

    def isErroneous = (tpe ne null) && tpe.isErroneous

    /** Apply `f' to each subtree */
    def foreach(f: Tree => Unit) { new ForeachTreeTraverser(f).traverse(this) }

    /** Find all subtrees matching predicate `p' */
    def filter(f: Tree => Boolean): List[Tree] = {
      val ft = new FilterTreeTraverser(f)
      ft.traverse(this)
      ft.hits.toList
    }

    /** Returns optionally first tree (in a preorder traversal) which satisfies predicate `p',
     *  or None if none exists. 
     */
    def find(p: Tree => Boolean): Option[Tree] = {
      val ft = new FindTreeTraverser(p)
      ft.traverse(this)
      ft.result
    }

    /** Is there part of this tree which satisfies predicate `p'? */
    def exists(p: Tree => Boolean): Boolean = !find(p).isEmpty

    /** The direct child trees of this tree
     *  EmptyTrees are always omitted. Lists are collapsed.
     */
    def children: List[Tree] = {
      def subtrees(x: Any): List[Tree] = x match {
        case EmptyTree => List()
        case t: Tree => List(t)
        case xs: List[_] => xs flatMap subtrees
        case _ => List()
      }
      productIterator.toList flatMap subtrees
    }

    override def toString(): String = 
      if (settings.Ycompacttrees.value) treePrinters.asCompactString(this)
      else treePrinters.asString(this)

    override def hashCode(): Int = super.hashCode()

    override def equals(that: Any): Boolean = that match {
      case t: Tree => this eq t
      case _ => false
    }
    def hashCodeStructure: Int = {
      var hc = getClass.hashCode
      def f(what : Any) : Unit = what match {
      case what : Tree => hc += what.hashCodeStructure
      case what : Iterable[_] => what.foreach(f)
      case what : Product => g(what)
      case null => 
      case what => hc += what.hashCode
      }
      def g(what: Product) {
        hc += what.productArity
        var i = 0
        while (i < what.productArity) {
          f(what.productElement(i))
          i += 1
        }
      }
      g(this)
      hc
    }
    def equalsStructure(that : Tree) = equalsStructure0(that){case (t0,t1) => false}
    def equalsStructure0(that: Tree)(f : (Tree,Tree) => Boolean): Boolean = {
      if (this == that) return true
      if (this.getClass != that.getClass) return false
      val this0 = this.asInstanceOf[Product]
      val that0 = that.asInstanceOf[Product]
      assert(this0.productArity == that0.productArity)
      def equals0(thiz: Any, that: Any): Boolean = thiz match {
        case thiz: Tree => 
          f(thiz,that.asInstanceOf[Tree]) || thiz.equalsStructure0(that.asInstanceOf[Tree])(f)
        case thiz: List[_] =>
          val that0 = that.asInstanceOf[List[Any]]
          if (thiz.length != that0.length) false
          else {
            val results0 = for (i <- 0.until(thiz.length).toList) 
              yield equals0(thiz(i), that0(i))
            results0.foldLeft(true)((x,y) => x && y)
          }
        case thiz =>
          thiz == that
      }
      val results = for (i <- 0.until(this0.productArity).toList) yield 
        equals0(this0.productElement(i), that0.productElement(i))
      val b = results.foldLeft(true)((x,y) => x && y)
      if (b) (this,that) match {
      case (this0 : TypeTree, that0 : TypeTree) if this0.original != null && that0.original != null =>  
        this0.original.equalsStructure0(that0.original)(f)
      case _ => true 
      } else false
    }

    /** Make a copy of this tree, keeping all attributes,
     *  except that all positions are focussed (so nothing
     *  in this tree will be found when searching by position).
     */
    def duplicate: this.type =
      (duplicator transform this).asInstanceOf[this.type]

    def shallowDuplicate: this.type =
      ((new ShallowDuplicator(this)) transform this).asInstanceOf[this.type]

    def copyAttrs(tree: Tree): this.type = {
      rawpos = tree.rawpos
      tpe = tree.tpe
      if (hasSymbol) symbol = tree.symbol
      this
    }
  }

  trait SymTree extends Tree {
    override def hasSymbol = true
    override var symbol: Symbol = NoSymbol
  }

  trait RefTree extends SymTree {
    def name: Name
  }

  abstract class DefTree extends SymTree {
    def name: Name
    override def isDef = true
  }

  trait TermTree extends Tree {
    override def isTerm = true
  }

  /** A tree for a type.  Note that not all type trees implement
    * this trait; in particular, Ident's are an exception. */
  trait TypTree extends Tree {
    override def isType = true
  }

// ----- auxiliary objects and methods ------------------------------

  private lazy val duplicator = new Transformer {
    override val treeCopy = new StrictTreeCopier
    override def transform(t: Tree) = {
      val t1 = super.transform(t)
      if ((t1 ne t) && t1.pos.isRange) t1 setPos t.pos.focus
      t1
    }
  }

  private class ShallowDuplicator(orig: Tree) extends Transformer {
    override val treeCopy = new StrictTreeCopier
    override def transform(tree: Tree) =
      if (tree eq orig)
	super.transform(tree)
      else
	tree
  }

//  def nextPhase = if (phase.id > globalPhase.id) phase else phase.next;

// ----- tree node alternatives --------------------------------------

  /** The empty tree */
  case object EmptyTree extends TermTree {
    super.tpe_=(NoType)
    override def tpe_=(t: Type) = 
      if (t != NoType) throw new Error("tpe_=("+t+") inapplicable for <empty>")
    override def resetType() {}
    override def isEmpty = true
  }

  abstract class MemberDef extends DefTree {
    def mods: Modifiers
    def keyword: String = this match {
      case TypeDef(_, _, _, _)      => "type"
      case ClassDef(mods, _, _, _)  => if (mods.isTrait) "trait" else "class"
      case DefDef(_, _, _, _, _, _) => "def"
      case ModuleDef(_, _, _)       => "object"
      case PackageDef(_, _)         => "package"
      case ValDef(mods, _, _, _)    => if (mods.isVariable) "var" else "val"
      case _ => ""
    }
    final def hasFlag(mask: Long): Boolean = (mods.flags & mask) != 0L
  }

  /** Package clause
   */
  case class PackageDef(pid: RefTree, stats: List[Tree])
       extends MemberDef {
    def name = pid.name
    def mods = NoMods
  }

/* disabled, as this is now dangerous
  def PackageDef(sym: Symbol, stats: List[Tree]): PackageDef =
    PackageDef(Ident(sym.name), stats) setSymbol sym
*/ 

  abstract class ImplDef extends MemberDef {
    def impl: Template
  }

  /** Class definition */
  case class ClassDef(mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template)
       extends ImplDef

  /**
   *  @param sym       the class symbol
   *  @param impl      ...
   *  @return          ...
   */
  def ClassDef(sym: Symbol, impl: Template): ClassDef =
    atPos(sym.pos) {
      ClassDef(Modifiers(sym.flags),
               sym.name,
               sym.typeParams map TypeDef,
               impl) setSymbol sym
    }

  /** Construct class definition with given class symbol, value parameters,
   *  supercall arguments and template body.
   *
   *  @param sym        the class symbol
   *  @param constrMods the modifiers for the class constructor, i.e. as in `class C private (...)'
   *  @param vparamss   the value parameters -- if they have symbols they
   *                    should be owned by `sym'
   *  @param argss      the supercall arguments
   *  @param body       the template statements without primary constructor
   *                    and value parameter fields.
   *  @return          ...
   */
  def ClassDef(sym: Symbol, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): ClassDef =
    ClassDef(sym, 
      Template(sym.info.parents map TypeTree, 
               if (sym.thisSym == sym || phase.erasedTypes) emptyValDef else ValDef(sym.thisSym),
               constrMods, vparamss, argss, body, superPos))

  /** Singleton object definition
   *
   *  @param mods
   *  @param name
   *  @param impl
   */
  case class ModuleDef(mods: Modifiers, name: Name, impl: Template)
       extends ImplDef

  /**
   *  @param sym       the class symbol
   *  @param impl      ...
   *  @return          ...
   */
  def ModuleDef(sym: Symbol, impl: Template): ModuleDef =
    atPos(sym.pos) {
      ModuleDef(Modifiers(sym.flags), sym.name, impl) setSymbol sym
    }

  abstract class ValOrDefDef extends MemberDef {
    def tpt: Tree
    def rhs: Tree
  }

  /** Value definition
   *
   *  @param mods
   *  @param name
   *  @param tpt
   *  @param rhs
   */
  case class ValDef(mods: Modifiers, name: Name, tpt: Tree, rhs: Tree)
       extends ValOrDefDef {
    assert(tpt.isType, tpt)
    //assert(kindingIrrelevant(tpt.tpe) || !tpt.tpe.isHigherKinded, tpt.tpe) //@M a value definition should never be typed with a higher-kinded type (values must be classified by types with kind *)
    //tpt.kindStar=true //@M turn on consistency checking in Tree
    assert(rhs.isTerm, rhs)
  }

  def ValDef(sym: Symbol, rhs: Tree): ValDef =
    atPos(sym.pos) {
      ValDef(Modifiers(sym.flags), sym.name, 
             TypeTree(sym.tpe) setPos sym.pos.focus, 
             rhs) setSymbol sym
    }

  def ValDef(sym: Symbol): ValDef = ValDef(sym, EmptyTree)

  object emptyValDef 
  extends ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree) {
    override def isEmpty = true
    super.setPos(NoPosition)
    override def setPos(pos: Position) = { assert(false); this }
  }

  /** Method definition
   *
   *  @param mods
   *  @param name
   *  @param tparams
   *  @param vparamss
   *  @param tpt
   *  @param rhs
   */
  case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
       extends ValOrDefDef {
    assert(tpt.isType, tpt)
    //assert(kindingIrrelevant(tpt.tpe) || !tpt.tpe.isHigherKinded, tpt.tpe) //@M a method definition should never be typed with a higher-kinded type (values must be classified by types with kind *)
    //tpt.kindStar=true //@M turn on consistency checking in Tree
    assert(rhs.isTerm, rhs)
  }

  def DefDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    atPos(sym.pos) {
      assert(sym != NoSymbol)
      DefDef(Modifiers(sym.flags),
             sym.name,
             sym.typeParams map TypeDef,
             vparamss,
             TypeTree(sym.tpe.finalResultType) setPos sym.pos.focus,
             rhs) setSymbol sym
    }

  def DefDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), vparamss, rhs)
  
  def DefDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef =
    DefDef(sym, mods, sym.paramss map (_.map(ValDef)), rhs)

  def DefDef(sym: Symbol, rhs: Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), rhs)

  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef = {
    DefDef(sym, rhs(sym.info.paramss))
  }

  /** Abstract type, type parameter, or type alias */
  case class TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) 
       extends MemberDef 

  /** A TypeDef node which defines given `sym' with given tight hand side `rhs'. */
  def TypeDef(sym: Symbol, rhs: Tree): TypeDef =
    atPos(sym.pos) {
      TypeDef(Modifiers(sym.flags), sym.name, sym.typeParams map TypeDef, rhs) setSymbol sym
    }

  /** A TypeDef node which defines abstract type or type parameter for given `sym' */
  def TypeDef(sym: Symbol): TypeDef = 
    TypeDef(sym, TypeBoundsTree(TypeTree(sym.info.bounds.lo), TypeTree(sym.info.bounds.hi)))

  /** <p>
   *    Labelled expression - the symbols in the array (must be Idents!) 
   *    are those the label takes as argument 
   *  </p>
   *  <p>
   *    The symbol that is given to the labeldef should have a MethodType
   *    (as if it were a nested function)
   *  </p>
   *  <p>
   *    Jumps are apply nodes attributed with label symbol, the arguments
   *    will get assigned to the idents.
   *  </p>
   *  <p>
   *  Note: on 2005-06-09 Martin, Iuli, Burak agreed to have forward
   *        jumps within a Block.
   *  </p>
   */
  case class LabelDef(name: Name, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree {
    assert(rhs.isTerm)
  }

  /**
   *  @param sym    the class symbol
   *  @param params ...
   *  @param rhs    ...
   *  @return       ...
   */
  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef =
    atPos(sym.pos) {
      LabelDef(sym.name, params map Ident, rhs) setSymbol sym
    }

  /** Import selector
   *
   * Representation of an imported name its optional rename and their optional positions
   * 
   * @param name      the imported name
   * @param namePos   its position or -1 if undefined
   * @param rename    the name the import is renamed to (== name if no renaming)
   * @param renamePos the position of the rename or -1 if undefined
   */
  case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)
  
  /** Import clause
   *
   *  @param expr
   *  @param selectors
   */
  case class Import(expr: Tree, selectors: List[ImportSelector])
       extends SymTree 
    // The symbol of an Import is an import symbol @see Symbol.newImport
    // It's used primarily as a marker to check that the import has been typechecked.

  /** Documented definition, eliminated by analyzer */
  case class DocDef(comment: DocComment, definition: Tree)
       extends Tree {
    override def symbol: Symbol = definition.symbol
    override def symbol_=(sym: Symbol) { definition.symbol = sym }
    // sean: seems to be important to the IDE
    override def isDef = definition.isDef
    override def isTerm = definition.isTerm
    override def isType = definition.isType
  }

  /** Instantiation template of a class or trait
   *
   *  @param parents
   *  @param body
   */
  case class Template(parents: List[Tree], self: ValDef, body: List[Tree])
       extends SymTree {
    // the symbol of a template is a local dummy. @see Symbol.newLocalDummy
    // the owner of the local dummy is the enclosing trait or class.
    // the local dummy is itself the owner of any local blocks
    // For example:
    //
    // class C { 
    //   def foo // owner is C 
    //   { 
    //      def bar  // owner is local dummy
    //   }
    // System.err.println("TEMPLATE: " + parents)
  }

  /** Generates a template with constructor corresponding to 
   *
   *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
   *  extends superclass(args_1) ... (args_n) with mixins { self => body }
   *
   *  This gets translated to
   *
   *  extends superclass with mixins { self =>
   *    presupers' // presupers without rhs
   *    vparamss   // abstract fields corresponding to value parameters
   *    def <init>(vparamss) { 
   *      presupers
   *      super.<init>(args)
   *    }
   *    body
   *  }
   */
  def Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree], superPos: Position): Template = {
    /* Add constructor to template */
    
    // create parameters for <init> as synthetic trees.
    var vparamss1 = 
      vparamss map (vps => vps.map { vd =>
        atPos(vd.pos.focus) {
          val pa = if (vd.hasFlag(PRIVATE | LOCAL)) 0L else PARAMACCESSOR
          ValDef(
            Modifiers(vd.mods.flags & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM) | PARAM | pa) withAnnotations vd.mods.annotations,
            vd.name, vd.tpt.duplicate, vd.rhs.duplicate)
        }})
    val (edefs, rest) = body span treeInfo.isEarlyDef
    val (evdefs, etdefs) = edefs partition treeInfo.isEarlyValDef
    val (lvdefs, gvdefs) = evdefs map {
      case vdef @ ValDef(mods, name, tpt, rhs) =>
        val fld = treeCopy.ValDef(
          vdef.duplicate, mods, name, 
          atPos(vdef.pos.focus) { TypeTree() setOriginal tpt setPos tpt.pos.focus }, // atPos in case 
          EmptyTree)
        val local = treeCopy.ValDef(vdef, Modifiers(PRESUPER), name, tpt, rhs)
        (local, fld)
    } unzip

    val constrs = {
      if (constrMods.isTrait) {
        if (body forall treeInfo.isInterfaceMember) List()
        else List(
          atPos(wrappingPos(superPos, lvdefs)) (
            DefDef(NoMods, nme.MIXIN_CONSTRUCTOR, List(), List(List()), TypeTree(), Block(lvdefs, Literal(())))))
      } else {
        // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
        if (vparamss1.isEmpty ||
            !vparamss1.head.isEmpty && (vparamss1.head.head.mods.flags & IMPLICIT) != 0L) 
          vparamss1 = List() :: vparamss1;
        val superRef: Tree = atPos(superPos) {
          Select(Super(nme.EMPTY.toTypeName, nme.EMPTY.toTypeName), nme.CONSTRUCTOR)
        }
        val superCall = (superRef /: argss) (Apply)
        List(
          atPos(wrappingPos(superPos, lvdefs ::: argss.flatten)) (
            DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), Block(lvdefs ::: List(superCall), Literal(())))))
      }
    } 
    // println("typed template, gvdefs = "+gvdefs+", parents = "+parents+", constrs = "+constrs)
    constrs foreach (ensureNonOverlapping(_, parents ::: gvdefs))
    // remove defaults
    val vparamss2 = vparamss map (vps => vps map { vd => 
      treeCopy.ValDef(vd, vd.mods &~ DEFAULTPARAM, vd.name, vd.tpt, EmptyTree)
    })
    Template(parents, self, gvdefs ::: vparamss2.flatten ::: constrs ::: etdefs ::: rest)
  }

  /** Block of expressions (semicolon separated expressions) */
  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree

  /** Case clause in a pattern match, eliminated by TransMatch 
   *  (except for occurences in switch statements)
   */
  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree

  /** casedef shorthand */
  def CaseDef(pat: Tree, body: Tree): CaseDef = CaseDef(pat, EmptyTree, body)

  /** Alternatives of patterns, eliminated by TransMatch, except for
   *  occurences in encoded Switch stmt (=remaining Match(CaseDef(...))
   */
  case class Alternative(trees: List[Tree])
       extends TermTree

  /** Repetition of pattern, eliminated by TransMatch */
  case class Star(elem: Tree)
       extends TermTree

  /** Bind of a variable to a rhs pattern, eliminated by TransMatch
   *
   *  @param name
   *  @param body
   */
  case class Bind(name: Name, body: Tree)
       extends DefTree {
    override def isTerm = name.isTermName
    override def isType = name.isTypeName
  }

  def Bind(sym: Symbol, body: Tree): Bind =
    Bind(sym.name, body) setSymbol sym

  case class UnApply(fun: Tree, args: List[Tree]) 
       extends TermTree

  /** Array of expressions, needs to be translated in backend,
   */
  case class ArrayValue(elemtpt: Tree, elems: List[Tree])
       extends TermTree

  /** Anonymous function, eliminated by analyzer */
  case class Function(vparams: List[ValDef], body: Tree)
       extends TermTree with SymTree 
    // The symbol of a Function is a synthetic value of name nme.ANON_FUN_NAME
    // It is the owner of the function's parameters.

  /** Assignment */
  case class Assign(lhs: Tree, rhs: Tree)
       extends TermTree

  /** Either an assignment or a named argument. Only appears in argument lists,
   *  eliminated by typecheck (doTypedApply)
   */
  case class AssignOrNamedArg(lhs: Tree, rhs: Tree)
       extends TermTree

  /** Conditional expression */
  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree

  /** <p>
   *    Pattern matching expression  (before <code>TransMatch</code>)
   *    Switch statements            (after TransMatch)
   *  </p>
   *  <p>
   *    After <code>TransMatch</code>, cases will satisfy the following
   *    constraints:
   *  </p>
   *  <ul>
   *    <li>all guards are EmptyTree,</li>
   *    <li>all patterns will be either <code>Literal(Constant(x:Int))</code>
   *      or <code>Alternative(lit|...|lit)</code></li>
   *    <li>except for an "otherwise" branch, which has pattern
   *      <code>Ident(nme.WILDCARD)</code></li>
   *  </ul>
   */
  case class Match(selector: Tree, cases: List[CaseDef])
       extends TermTree

  /** Return expression */
  case class Return(expr: Tree)
       extends TermTree with SymTree
    // The symbol of a Return node is the enclosing method.

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)
       extends TermTree

  /** Throw expression */
  case class Throw(expr: Tree)
       extends TermTree

  /** Object instantiation
   *  One should always use factory method below to build a user level new.
   *
   *  @param tpt    a class type
   */
  case class New(tpt: Tree)
       extends TermTree {
    assert(tpt.isType)
  }

  /** Factory method for object creation <code>&lt;new tpt(args_1)...(args_n)&gt;</code>.
   *  A New(t, as) is expanded to:
   *    (new t).<init>(as)
   *
   *  @param tpt   ...
   *  @param argss ...
   *  @return      ...
   */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = {
    assert(!argss.isEmpty)
    val superRef: Tree = Select(New(tpt), nme.CONSTRUCTOR)
    (superRef /: argss) (Apply)
  }

  /** Type annotation, eliminated by explicit outer */
  case class Typed(expr: Tree, tpt: Tree)
       extends TermTree

  // Martin to Sean: Should GenericApply/TypeApply/Apply not be SymTree's? After all, 
  // ApplyDynamic is a SymTree.
  abstract class GenericApply extends TermTree {
    val fun: Tree
    val args: List[Tree]
  }

  /** Type application */
  case class TypeApply(fun: Tree, args: List[Tree])
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }

  /** Value application */
  case class Apply(fun: Tree, args: List[Tree])
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }

  /** Dynamic value application. 
   *  In a dynamic application   q.f(as)
   *   - q is stored in qual
   *   - as is stored in args
   *   - f is stored as the node's symbol field.
   */
  case class ApplyDynamic(qual: Tree, args: List[Tree]) 
       extends TermTree with SymTree
    // The symbol of an ApplyDynamic is the function symbol of `qual', or NoSymbol, if there is none.

  /** Super reference */
  case class Super(qual: Name, mix: Name)
       extends TermTree with SymTree
    // The symbol of a Super is the class _from_ which the super reference is made.
    // For instance in C.super(...), it would be C.

  def Super(sym: Symbol, mix: Name): Tree = Super(sym.name, mix) setSymbol sym

  /** Self reference */
  case class This(qual: Name)
        extends TermTree with SymTree {
    // The symbol of a This is the class to which the this refers.
    // For instance in C.this, it would be C.
  }

  def This(sym: Symbol): Tree = This(sym.name) setSymbol sym

  /** Designator <qualifier> . <name> */
  case class Select(qualifier: Tree, name: Name)
       extends RefTree {
    override def isTerm = name.isTermName
    override def isType = name.isTypeName
  }

  def Select(qualifier: Tree, sym: Symbol): Select =
    Select(qualifier, sym.name) setSymbol sym

  /** Identifier <name> */
  case class Ident(name: Name)
       extends RefTree {
    override def isTerm = name.isTermName
    override def isType = name.isTypeName
  }

  class BackQuotedIdent(name: Name) extends Ident(name)

  def Ident(sym: Symbol): Ident =
    Ident(sym.name) setSymbol sym

  /** Literal */
  case class Literal(value: Constant)
        extends TermTree {
    assert(value ne null)
  }

  def Literal(value: Any): Literal =
    Literal(Constant(value))

  /** A synthetic term holding an arbitrary type.  Not to be confused with
    * with TypTree, the trait for trees that are only used for type trees.
    * TypeTree's are inserted in several places, but most notably in
    * <code>RefCheck</code>, where the arbitrary type trees are all replaced by
    * TypeTree's. */
  case class TypeTree() extends TypTree {
    override def symbol = if (tpe == null) null else tpe.typeSymbol

    private var orig: Tree = null 
    private var wasEmpty: Boolean = false

    def original: Tree = orig

    def setOriginal(tree: Tree): this.type = { orig = tree; setPos(tree.pos); this }

    override def defineType(tp: Type): this.type = {
      wasEmpty = isEmpty
      setType(tp)
    }

    /** Reset type to null, unless type original was empty and then
     *  got its type via a defineType
     */
    override def resetType() {
      if (wasEmpty) tpe = null
    }

    override def isEmpty = (tpe eq null) || tpe == NoType
  }

  def TypeTree(tp: Type): TypeTree = TypeTree() setType tp
  // def TypeTree(tp: Type, tree : Tree): TypeTree = TypeTree(tree) setType tp

  /** A tree that has an annotation attached to it. Only used for annotated types and
   *  annotation ascriptions, annotations on definitions are stored in the Modifiers.
   *  Eliminated by typechecker (typedAnnotated), the annotations are then stored in
   *  an AnnotatedType.
   */
  case class Annotated(annot: Tree, arg: Tree) extends Tree {
    override def isType = arg.isType
    override def isTerm = arg.isTerm
  }

  /** Singleton type, eliminated by RefCheck */
  case class SingletonTypeTree(ref: Tree)
        extends TypTree

  /** Type selection <qualifier> # <name>, eliminated by RefCheck */
  case class SelectFromTypeTree(qualifier: Tree, name: Name)
       extends TypTree with RefTree

  /** Intersection type <parent1> with ... with <parentN> { <decls> }, eliminated by RefCheck */
  case class CompoundTypeTree(templ: Template)
       extends TypTree

  /** Applied type <tpt> [ <args> ], eliminated by RefCheck */
  case class AppliedTypeTree(tpt: Tree, args: List[Tree])
       extends TypTree {
    override def symbol: Symbol = tpt.symbol
    override def symbol_=(sym: Symbol) { tpt.symbol = sym }
  }

  case class TypeBoundsTree(lo: Tree, hi: Tree)
       extends TypTree

  case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree])
       extends TypTree

  case class Parens(args: List[Tree]) extends Tree // only used during parsing

  /** Array selection <qualifier> . <name> only used during erasure */
  case class SelectFromArray(qualifier: Tree, name: Name, erasure: Type)
       extends TermTree with RefTree

  trait StubTree extends Tree {
    def underlying : AnyRef
    override def equalsStructure0(that: Tree)(f : (Tree,Tree) => Boolean): Boolean = this eq that
  }

/* A standard pattern match 
  case EmptyTree =>
  case PackageDef(pid, stats) =>
     // package pid { stats }
  case ClassDef(mods, name, tparams, impl) =>
     // mods class name [tparams] impl   where impl = extends parents { defs }
  case ModuleDef(mods, name, impl) =>                             (eliminated by refcheck)
     // mods object name impl  where impl = extends parents { defs }
  case ValDef(mods, name, tpt, rhs) =>
     // mods val name: tpt = rhs   
     // note missing type information is expressed by tpt = TypeTree()
  case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
     // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
     // note missing type information is expressed by tpt = TypeTree()
  case TypeDef(mods, name, tparams, rhs) =>                       (eliminated by erasure)
     // mods type name[tparams] = rhs
     // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree, 
                                              and DEFERRED is set in mods
  case LabelDef(name, params, rhs) =>
     // used for tailcalls and like
     // while/do are desugared to label defs as follows:
     // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
     // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
  case Import(expr, selectors) =>                                 (eliminated by typecheck)
     // import expr.{selectors}
     // Selectors are a list of pairs of names (from, to).
     // The last (and maybe only name) may be a nme.WILDCARD
     // for instance
     //   import qual.{x, y => z, _}  would be represented as
     //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
  case DocDef(comment, definition) =>                             (eliminated by typecheck)
     // /** comment */ definition
  case Template(parents, self, body) =>
     // extends parents { self => body }
     // if self is missing it is represented as emptyValDef
  case Block(stats, expr) =>
     // { stats; expr }
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch/explicitouter)
    // case pat if guard => body
  case Alternative(trees) =>                                      (eliminated by transmatch/explicitouter)
    // pat1 | ... | patn
  case Star(elem) =>                                              (eliminated by transmatch/explicitouter)
    // pat*
  case Bind(name, body) =>                                        (eliminated by transmatch/explicitouter)
    // name @ pat
  case UnApply(fun: Tree, args)                                   (introduced by typer, eliminated by transmatch/explicitouter)
    // used for unapply's
  case ArrayValue(elemtpt, trees) =>                              (introduced by uncurry)
    // used to pass arguments to vararg arguments
    // for instance, printf("%s%d", foo, 42) is translated to after uncurry to:
    // Apply(
    //   Ident("printf"), 
    //   Literal("%s%d"), 
    //   ArrayValue(<Any>, List(Ident("foo"), Literal(42))))
  case Function(vparams, body) =>                                 (eliminated by lambdaLift)
    // vparams => body  where vparams:List[ValDef]
  case Assign(lhs, rhs) =>
    // lhs = rhs
  case AssignOrNamedArg(lhs, rhs) =>                              (eliminated by typecheck)
    // lhs = rhs
  case If(cond, thenp, elsep) =>
    // if (cond) thenp else elsep
  case Match(selector, cases) =>
    // selector match { cases }
  case Return(expr) =>
    // return expr
  case Try(block, catches, finalizer) =>
    // try block catch { catches } finally finalizer where catches: List[CaseDef]
  case Throw(expr) =>
    // throw expr
  case New(tpt) =>
    // new tpt   always in the context: (new tpt).<init>[targs](args)
  case Typed(expr, tpt) =>                                        (eliminated by erasure)
    // expr: tpt
  case TypeApply(fun, args) =>
    // fun[args]
  case Apply(fun, args) =>
    // fun(args)
    // for instance fun[targs](args)  is expressed as  Apply(TypeApply(fun, targs), args)
  case ApplyDynamic(qual, args)                                   (introduced by erasure, eliminated by cleanup)
    // fun(args)
  case Super(qual, mix) =>
    // qual.super[mix]     if qual and/or mix is empty, ther are nme.EMPTY.toTypeName
  case This(qual) =>
    // qual.this
  case Select(qualifier, selector) =>
    // qualifier.selector
  case Ident(name) =>
    // name
    // note: type checker converts idents that refer to enclosing fields or methods
    // to selects; name ==> this.name
  case Literal(value) =>
    // value
  case TypeTree() =>                                              (introduced by refcheck)
    // a type that's not written out, but given in the tpe attribute
  case Annotated(annot, arg) =>                                   (eliminated by typer)
    // arg @annot  for types,  arg: @annot for exprs
  case SingletonTypeTree(ref) =>                                  (eliminated by uncurry)
    // ref.type
  case SelectFromTypeTree(qualifier, selector) =>                 (eliminated by uncurry)
    // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
  case CompoundTypeTree(templ: Template) =>                       (eliminated by uncurry)
    // parent1 with ... with parentN { refinement }
  case AppliedTypeTree(tpt, args) =>                              (eliminated by uncurry)
    // tpt[args]
  case TypeBoundsTree(lo, hi) =>                                  (eliminated by uncurry)
    // >: lo <: hi
  case ExistentialTypeTree(tpt, whereClauses) =>                  (eliminated by uncurry)
    // tpt forSome { whereClauses }

*/

  abstract class TreeCopier {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template): ClassDef
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]): PackageDef
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template): ModuleDef
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree): ValDef
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree): TypeDef
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]): Import
    def DocDef(tree: Tree, comment: DocComment, definition: Tree): DocDef
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]): Template
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef
    def Alternative(tree: Tree, trees: List[Tree]): Alternative
    def Star(tree: Tree, elem: Tree): Star
    def Bind(tree: Tree, name: Name, body: Tree): Bind
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]): UnApply
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]): ArrayValue
    def Function(tree: Tree, vparams: List[ValDef], body: Tree): Function
    def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree): AssignOrNamedArg
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]): Match
    def Return(tree: Tree, expr: Tree): Return
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree): Try
    def Throw(tree: Tree, expr: Tree): Throw
    def New(tree: Tree, tpt: Tree): New
    def Typed(tree: Tree, expr: Tree, tpt: Tree): Typed
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply
    def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]): ApplyDynamic
    def Super(tree: Tree, qual: Name, mix: Name): Super
    def This(tree: Tree, qual: Name): This
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select
    def Ident(tree: Tree, name: Name): Ident
    def Literal(tree: Tree, value: Constant): Literal
    def TypeTree(tree: Tree): TypeTree
    def Annotated(tree: Tree, annot: Tree, arg: Tree): Annotated
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree
    def CompoundTypeTree(tree: Tree, templ: Template): CompoundTypeTree
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree): TypeBoundsTree
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]): ExistentialTypeTree
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type): SelectFromArray
  }

  class StrictTreeCopier extends TreeCopier {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) =
      new ClassDef(mods, name, tparams, impl).copyAttrs(tree)
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]) =
      new PackageDef(pid, stats).copyAttrs(tree)
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) =
      new ModuleDef(mods, name, impl).copyAttrs(tree)
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) =
      new ValDef(mods, name, tpt, rhs).copyAttrs(tree)
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
      new DefDef(mods, name, tparams, vparamss, tpt, rhs).copyAttrs(tree)
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =
      new TypeDef(mods, name, tparams, rhs).copyAttrs(tree)
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      new LabelDef(name, params, rhs).copyAttrs(tree)
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]) =
      new Import(expr, selectors).copyAttrs(tree)
    def DocDef(tree: Tree, comment: DocComment, definition: Tree) =
      new DocDef(comment, definition).copyAttrs(tree)
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) =
      new Template(parents, self, body).copyAttrs(tree)
    def Block(tree: Tree, stats: List[Tree], expr: Tree) =
      new Block(stats, expr).copyAttrs(tree)
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
      new CaseDef(pat, guard, body).copyAttrs(tree)
    def Alternative(tree: Tree, trees: List[Tree]) =
      new Alternative(trees).copyAttrs(tree)
    def Star(tree: Tree, elem: Tree) =
      new Star(elem).copyAttrs(tree)
    def Bind(tree: Tree, name: Name, body: Tree) =
      new Bind(name, body).copyAttrs(tree)
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) = 
      new UnApply(fun, args).copyAttrs(tree)
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) =
      new ArrayValue(elemtpt, trees).copyAttrs(tree)
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
      new Function(vparams, body).copyAttrs(tree)
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
      new Assign(lhs, rhs).copyAttrs(tree)
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) =
      new AssignOrNamedArg(lhs, rhs).copyAttrs(tree)
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) =
      new If(cond, thenp, elsep).copyAttrs(tree)
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =
      new Match(selector, cases).copyAttrs(tree)
    def Return(tree: Tree, expr: Tree) =
      new Return(expr).copyAttrs(tree)
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) =
      new Try(block, catches, finalizer).copyAttrs(tree)
    def Throw(tree: Tree, expr: Tree) =
      new Throw(expr).copyAttrs(tree)
    def New(tree: Tree, tpt: Tree) =
      new New(tpt).copyAttrs(tree)
    def Typed(tree: Tree, expr: Tree, tpt: Tree) =
      new Typed(expr, tpt).copyAttrs(tree)
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) =
      new TypeApply(fun, args).copyAttrs(tree)
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) =
      new Apply(fun, args).copyAttrs(tree)
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) =
      new ApplyDynamic(qual, args).copyAttrs(tree)
    def Super(tree: Tree, qual: Name, mix: Name) =
      new Super(qual, mix).copyAttrs(tree)
    def This(tree: Tree, qual: Name) =
      new This(qual).copyAttrs(tree)
    def Select(tree: Tree, qualifier: Tree, selector: Name) =
      new Select(qualifier, selector).copyAttrs(tree)
    def Ident(tree: Tree, name: Name) =
      new Ident(name).copyAttrs(tree)
    def Literal(tree: Tree, value: Constant) =
      new Literal(value).copyAttrs(tree)
    def TypeTree(tree: Tree) =
      new TypeTree().copyAttrs(tree)
    def Annotated(tree: Tree, annot: Tree, arg: Tree) =
      new Annotated(annot, arg).copyAttrs(tree)
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      new SingletonTypeTree(ref).copyAttrs(tree)
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      new SelectFromTypeTree(qualifier, selector).copyAttrs(tree)
    def CompoundTypeTree(tree: Tree, templ: Template) =
      new CompoundTypeTree(templ).copyAttrs(tree)
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
      new AppliedTypeTree(tpt, args).copyAttrs(tree)
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) =
      new TypeBoundsTree(lo, hi).copyAttrs(tree)
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) =
      new ExistentialTypeTree(tpt, whereClauses).copyAttrs(tree)
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) =
      new SelectFromArray(qualifier, selector, erasure).copyAttrs(tree)
  }

  class LazyTreeCopier(treeCopy: TreeCopier) extends TreeCopier {
    def this() = this(new StrictTreeCopier)
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) = tree match {
      case t @ ClassDef(mods0, name0, tparams0, impl0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) && (impl0 == impl) => t
      case _ => treeCopy.ClassDef(tree, mods, name, tparams, impl)
    }
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]) = tree match {
      case t @ PackageDef(pid0, stats0)
      if (pid0 == pid) && (stats0 == stats) => t
      case _ => treeCopy.PackageDef(tree, pid, stats)
    }
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) = tree match {
      case t @ ModuleDef(mods0, name0, impl0)
      if (mods0 == mods) && (name0 == name) && (impl0 == impl) => t
      case _ => treeCopy.ModuleDef(tree, mods, name, impl)
    }
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) = tree match {
      case t @ ValDef(mods0, name0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tpt0 == tpt) && (rhs0 == rhs) => t
      case _ => treeCopy.ValDef(tree, mods, name, tpt, rhs)
    }
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) = tree match {
      case t @ DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) &&
         (vparamss0 == vparamss) && (tpt0 == tpt) && (rhs == rhs0) => t
      case _ => treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
    }
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) = tree match {
      case t @ TypeDef(mods0, name0, tparams0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) && (rhs0 == rhs) => t
      case _ => treeCopy.TypeDef(tree, mods, name, tparams, rhs)
    }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) = tree match {
      case t @ LabelDef(name0, params0, rhs0)
      if (name0 == name) && (params0 == params) && (rhs0 == rhs) => t
      case _ => treeCopy.LabelDef(tree, name, params, rhs)
    }
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]) = tree match {
      case t @ Import(expr0, selectors0)
      if (expr0 == expr) && (selectors0 == selectors) => t
      case _ => treeCopy.Import(tree, expr, selectors)
    }
    def DocDef(tree: Tree, comment: DocComment, definition: Tree) = tree match {
      case t @ DocDef(comment0, definition0)
      if (comment0 == comment) && (definition0 == definition) => t
      case _ => treeCopy.DocDef(tree, comment, definition)
    }
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) = tree match {
      case t @ Template(parents0, self0, body0)
      if (parents0 == parents) && (self0 == self) && (body0 == body) => t
      case _ => treeCopy.Template(tree, parents, self, body)
    }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) = tree match {
      case t @ Block(stats0, expr0)
      if ((stats0 == stats) && (expr0 == expr)) => t
      case _ => treeCopy.Block(tree, stats, expr)
    }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) = tree match {
      case t @ CaseDef(pat0, guard0, body0)
      if (pat0 == pat) && (guard0 == guard) && (body0 == body) => t
      case _ => treeCopy.CaseDef(tree, pat, guard, body)
    }
    def Alternative(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Alternative(trees0)
      if trees0 == trees => t
      case _ => treeCopy.Alternative(tree, trees)
    }
    def Star(tree: Tree, elem: Tree) = tree match {
      case t @ Star(elem0)
      if elem0 == elem => t
      case _ => treeCopy.Star(tree, elem)
    }
    def Bind(tree: Tree, name: Name, body: Tree) = tree match {
      case t @ Bind(name0, body0)
      if (name0 == name) && (body0 == body) => t
      case _ => treeCopy.Bind(tree, name, body)
    }
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ UnApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.UnApply(tree, fun, args)
    }
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) = tree match {
      case t @ ArrayValue(elemtpt0, trees0)
      if (elemtpt0 == elemtpt) && (trees0 == trees) => t
      case _ => treeCopy.ArrayValue(tree, elemtpt, trees)
    }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) = tree match {
      case t @ Function(vparams0, body0)
      if (vparams0 == vparams) && (body0 == body) => t
      case _ => treeCopy.Function(tree, vparams, body)
    }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ Assign(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => treeCopy.Assign(tree, lhs, rhs)
    }
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ AssignOrNamedArg(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => treeCopy.AssignOrNamedArg(tree, lhs, rhs)
    }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) = tree match {
      case t @ If(cond0, thenp0, elsep0) 
      if (cond0 == cond) && (thenp0 == thenp) && (elsep0 == elsep) => t
      case _ => treeCopy.If(tree, cond, thenp, elsep)
    }
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =  tree match {
      case t @ Match(selector0, cases0)
      if (selector0 == selector) && (cases0 == cases) => t
      case _ => treeCopy.Match(tree, selector, cases)
    }
    def Return(tree: Tree, expr: Tree) = tree match {
      case t @ Return(expr0)
      if expr0 == expr => t
      case _ => treeCopy.Return(tree, expr)
    }
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) = tree match {
      case t @ Try(block0, catches0, finalizer0)
      if (block0 == block) && (catches0 == catches) && (finalizer0 == finalizer) => t
      case _ => treeCopy.Try(tree, block, catches, finalizer)
    }
    def Throw(tree: Tree, expr: Tree) = tree match {
      case t @ Throw(expr0)
      if expr0 == expr => t
      case _ => treeCopy.Throw(tree, expr)
    }
    def New(tree: Tree, tpt: Tree) = tree match {
      case t @ New(tpt0)
      if tpt0 == tpt => t
      case _ => treeCopy.New(tree, tpt)
    }
    def Typed(tree: Tree, expr: Tree, tpt: Tree) = tree match {
      case t @ Typed(expr0, tpt0)
      if (expr0 == expr) && (tpt0 == tpt) => t
      case _ => treeCopy.Typed(tree, expr, tpt)
    }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ TypeApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.TypeApply(tree, fun, args)
    }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ Apply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.Apply(tree, fun, args)
    }
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) = tree match {
      case t @ ApplyDynamic(qual0, args0)
      if (qual0 == qual) && (args0 == args) => t
      case _ => treeCopy.ApplyDynamic(tree, qual, args)
    }
    def Super(tree: Tree, qual: Name, mix: Name) = tree match {
      case t @ Super(qual0, mix0)
      if (qual0 == qual) && (mix0 == mix) => t
      case _ => treeCopy.Super(tree, qual, mix)
    }
    def This(tree: Tree, qual: Name) = tree match {
      case t @ This(qual0)
      if qual0 == qual => t
      case _ => treeCopy.This(tree, qual)
    }
    def Select(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.Select(tree, qualifier, selector)
    }
    def Ident(tree: Tree, name: Name) = tree match {
      case t @ Ident(name0)
      if name0 == name => t
      case _ => treeCopy.Ident(tree, name)
    }
    def Literal(tree: Tree, value: Constant) = tree match {
      case t @ Literal(value0)
      if value0 == value => t
      case _ => treeCopy.Literal(tree, value)
    }
    def TypeTree(tree: Tree) = tree match {
      case t @ TypeTree() => t
      case _ => treeCopy.TypeTree(tree)
    }
    def Annotated(tree: Tree, annot: Tree, arg: Tree) = tree match {
      case t @ Annotated(annot0, arg0)
      if (annot0==annot) => t
      case _ => treeCopy.Annotated(tree, annot, arg)
    }
    def SingletonTypeTree(tree: Tree, ref: Tree) = tree match {
      case t @ SingletonTypeTree(ref0)
      if ref0 == ref => t
      case _ => treeCopy.SingletonTypeTree(tree, ref)
    }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ SelectFromTypeTree(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.SelectFromTypeTree(tree, qualifier, selector)
    }
    def CompoundTypeTree(tree: Tree, templ: Template) = tree match {
      case t @ CompoundTypeTree(templ0)
      if templ0 == templ => t
      case _ => treeCopy.CompoundTypeTree(tree, templ)
    }
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) = tree match {
      case t @ AppliedTypeTree(tpt0, args0)
      if (tpt0 == tpt) && (args0 == args) => t
      case _ => treeCopy.AppliedTypeTree(tree, tpt, args)
    }
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) = tree match {
      case t @ TypeBoundsTree(lo0, hi0)
      if (lo0 == lo) && (hi0 == hi) => t
      case _ => treeCopy.TypeBoundsTree(tree, lo, hi)
    }
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) = tree match {
      case t @ ExistentialTypeTree(tpt0, whereClauses0)
      if (tpt0 == tpt) && (whereClauses0 == whereClauses) => t
      case _ => treeCopy.ExistentialTypeTree(tree, tpt, whereClauses)
    }
    def SelectFromArray(tree: Tree, qualifier: Tree, selector: Name, erasure: Type) = tree match {
      case t @ SelectFromArray(qualifier0, selector0, _)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.SelectFromArray(tree, qualifier, selector, erasure)
    }
  }

  abstract class Transformer {
    val treeCopy: TreeCopier = new LazyTreeCopier
    protected var currentOwner: Symbol = definitions.RootClass
    protected def currentMethod = currentOwner.enclMethod
    protected def currentClass = currentOwner.enclClass
    protected def currentPackage = currentOwner.toplevelClass.owner
    def transform(tree: Tree): Tree = tree match {
      case EmptyTree =>
        tree
      case PackageDef(pid, stats) =>
        treeCopy.PackageDef(
          tree, transform(pid).asInstanceOf[RefTree], 
          atOwner(tree.symbol.moduleClass) {
            transformStats(stats, currentOwner)
          }
        )
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          treeCopy.ClassDef(tree, transformModifiers(mods), name,
                            transformTypeDefs(tparams), transformTemplate(impl))
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(tree.symbol.moduleClass) {
          treeCopy.ModuleDef(tree, transformModifiers(mods),
                             name, transformTemplate(impl))
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.ValDef(tree, transformModifiers(mods),
                          name, transform(tpt), transform(rhs))
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.DefDef(tree, transformModifiers(mods), name,
                          transformTypeDefs(tparams), transformValDefss(vparamss),
                          transform(tpt), transform(rhs))
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.TypeDef(tree, transformModifiers(mods), name,
                           transformTypeDefs(tparams), transform(rhs))
        }
      case LabelDef(name, params, rhs) =>
        treeCopy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LamdaLifter.proxy'
      case Import(expr, selectors) =>
        treeCopy.Import(tree, transform(expr), selectors)
      case DocDef(comment, definition) =>
        treeCopy.DocDef(tree, comment, transform(definition))
      case Template(parents, self, body) =>
        treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body, tree.symbol))
      case Block(stats, expr) =>
        treeCopy.Block(tree, transformStats(stats, currentOwner), transform(expr))
      case CaseDef(pat, guard, body) =>
        treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Alternative(trees) =>
        treeCopy.Alternative(tree, transformTrees(trees))
      case Star(elem) =>
        treeCopy.Star(tree, transform(elem))
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case UnApply(fun, args) =>
        treeCopy.UnApply(tree, fun, transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
      case ArrayValue(elemtpt, trees) =>
        treeCopy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          treeCopy.Function(tree, transformValDefs(vparams), transform(body))
        }
      case Assign(lhs, rhs) =>
        treeCopy.Assign(tree, transform(lhs), transform(rhs))
      case AssignOrNamedArg(lhs, rhs) =>
        treeCopy.AssignOrNamedArg(tree, transform(lhs), transform(rhs))
      case If(cond, thenp, elsep) =>
        treeCopy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
        treeCopy.Match(tree, transform(selector), transformCaseDefs(cases))
      case Return(expr) =>
        treeCopy.Return(tree, transform(expr))
      case Try(block, catches, finalizer) =>
        treeCopy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case Throw(expr) =>
        treeCopy.Throw(tree, transform(expr))
      case New(tpt) =>
        treeCopy.New(tree, transform(tpt))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), transform(tpt))
      case TypeApply(fun, args) =>
        treeCopy.TypeApply(tree, transform(fun), transformTrees(args))
      case Apply(fun, args) =>
        treeCopy.Apply(tree, transform(fun), transformTrees(args))
      case ApplyDynamic(qual, args) =>
        treeCopy.ApplyDynamic(tree, transform(qual), transformTrees(args))
      case Super(qual, mix) =>
        treeCopy.Super(tree, qual, mix)
      case This(qual) =>
        treeCopy.This(tree, qual)
      case Select(qualifier, selector) =>
        treeCopy.Select(tree, transform(qualifier), selector)
      case Ident(name) =>
        treeCopy.Ident(tree, name)
      case Literal(value) =>
        treeCopy.Literal(tree, value)
      case TypeTree() =>
        treeCopy.TypeTree(tree)
      case Annotated(annot, arg) =>
        treeCopy.Annotated(tree, transform(annot), transform(arg))
      case SingletonTypeTree(ref) =>
        treeCopy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        treeCopy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        treeCopy.CompoundTypeTree(tree, transformTemplate(templ))
      case AppliedTypeTree(tpt, args) =>
        treeCopy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
      case TypeBoundsTree(lo, hi) =>
        treeCopy.TypeBoundsTree(tree, transform(lo), transform(hi)) 
      case ExistentialTypeTree(tpt, whereClauses) =>
        treeCopy.ExistentialTypeTree(tree, transform(tpt), transformTrees(whereClauses))
      case SelectFromArray(qualifier, selector, erasure) =>
        treeCopy.SelectFromArray(tree, transform(qualifier), selector, erasure)
      case tree : StubTree => 
        tree.symbol = NoSymbol
        tree.tpe = null
        tree
    }

    def transformTrees(trees: List[Tree]): List[Tree] =
        trees mapConserve (transform(_))
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template]
    def transformTypeDefs(trees: List[TypeDef]): List[TypeDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[TypeDef])
    def transformValDef(tree: ValDef): ValDef =
      if (tree.isEmpty) tree else transform(tree).asInstanceOf[ValDef]
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      trees mapConserve (transformValDef(_))
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      treess mapConserve (transformValDefs(_))
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[CaseDef])
    def transformIdents(trees: List[Ident]): List[Ident] =
      trees mapConserve (tree => transform(tree).asInstanceOf[Ident])
    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      stats mapConserve (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(transform(stat))
        else transform(stat)) filter (EmptyTree !=)
    def transformUnit(unit: CompilationUnit) { unit.body = transform(unit.body) }
    def transformModifiers(mods: Modifiers): Modifiers =
      Modifiers(mods.flags, mods.privateWithin, transformTrees(mods.annotations), mods.positions)

    def atOwner[A](owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner
      val result = trans
      currentOwner = prevOwner
      result
    }
  }

  class Traverser {
    protected var currentOwner: Symbol = definitions.RootClass
    def traverse(tree: Tree): Unit =  tree match {
      case EmptyTree =>
        ;
      case PackageDef(pid, stats) =>
        traverse(pid)
        atOwner(tree.symbol.moduleClass) {
          traverseTrees(stats)
        }
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(impl)
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(tree.symbol.moduleClass) {
          traverseTrees(mods.annotations); traverse(impl)
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverse(tpt); traverse(rhs)
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverseTreess(vparamss); traverse(tpt); traverse(rhs)
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(rhs)
        }
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case Annotated(annot, arg) =>
        traverse(annot); traverse(arg)
      case DocDef(comment, definition) =>
        traverse(definition) 
      case Template(parents, self, body) =>
        traverseTrees(parents)
        if (!self.isEmpty) traverse(self)
        traverseStats(body, tree.symbol)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Star(elem) =>
        traverse(elem)
      case Bind(name, body) =>
        traverse(body)
      case UnApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ArrayValue(elemtpt, trees) =>
        traverse(elemtpt); traverseTrees(trees)
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          traverseTrees(vparams); traverse(body)
        }
      case Assign(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case AssignOrNamedArg(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case If(cond, thenp, elsep) =>
        traverse(cond); traverse(thenp); traverse(elsep)
      case Match(selector, cases) =>
        traverse(selector); traverseTrees(cases)
      case Return(expr) =>
        traverse(expr)
      case Try(block, catches, finalizer) =>
        traverse(block); traverseTrees(catches); traverse(finalizer)
      case Throw(expr) =>
        traverse(expr)
      case New(tpt) =>
        traverse(tpt)
      case Typed(expr, tpt) =>
        traverse(expr); traverse(tpt)
      case TypeApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case Apply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ApplyDynamic(qual, args) =>
        traverse(qual); traverseTrees(args)
      case Super(_, _) =>
        ;
      case This(_) =>
        ;
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case Ident(_) =>
        ;
      case Literal(_) =>
        ;
      case TypeTree() =>
        ;
      case SingletonTypeTree(ref) =>
        traverse(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        traverse(qualifier)
      case CompoundTypeTree(templ) =>
        traverse(templ)
      case AppliedTypeTree(tpt, args) =>
        traverse(tpt); traverseTrees(args)
      case TypeBoundsTree(lo, hi) =>
        traverse(lo); traverse(hi)
      case ExistentialTypeTree(tpt, whereClauses) =>
        traverse(tpt); traverseTrees(whereClauses)
      case SelectFromArray(qualifier, selector, erasure) =>
        traverse(qualifier)
      case Parens(ts) =>
        traverseTrees(ts)
      case tree : StubTree =>
    }

    def traverseTrees(trees: List[Tree]) {
      trees foreach traverse
    }
    def traverseTreess(treess: List[List[Tree]]) {
      treess foreach traverseTrees
    }
    def traverseStats(stats: List[Tree], exprOwner: Symbol) {
      stats foreach (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(traverse(stat))
        else traverse(stat))
    }
    def apply[T <: Tree](tree: T): T = { traverse(tree); tree }

    def atOwner(owner: Symbol)(traverse: => Unit) {
      val prevOwner = currentOwner
      currentOwner = owner
      traverse
      currentOwner = prevOwner
    }
  }

  class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(_) =>
        def subst(from: List[Symbol], to: List[Tree]): Tree =
          if (from.isEmpty) tree
          else if (tree.symbol == from.head) to.head
          else subst(from.tail, to.tail);
        subst(from, to)
      case _ =>
        super.transform(tree)
    }
  }

  class TreeTypeSubstituter(val from: List[Symbol], val to: List[Type]) extends Traverser {
    val typeSubst = new SubstTypeMap(from, to)
    override def traverse(tree: Tree) {
      if (tree.tpe ne null) tree.tpe = typeSubst(tree.tpe)
      if (tree.isDef) {
        val sym = tree.symbol
        val info1 = typeSubst(sym.info)
        if (info1 ne sym.info) sym.setInfo(info1)
      }
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
    override def toString() = "TreeTypeSubstituter("+from+","+to+")"
  }

  lazy val EmptyTreeTypeSubstituter = new TreeTypeSubstituter(List(), List())

  class TreeSymSubstituter(from: List[Symbol], to: List[Symbol]) extends Traverser {
    val symSubst = new SubstSymMap(from, to)
    override def traverse(tree: Tree) {
      def subst(from: List[Symbol], to: List[Symbol]) {
        if (!from.isEmpty)
          if (tree.symbol == from.head) tree setSymbol to.head
          else subst(from.tail, to.tail)
      }
      if (tree.tpe ne null) tree.tpe = symSubst(tree.tpe)
      if (tree.hasSymbol) subst(from, to)
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
    override def toString() = "TreeSymSubstituter("+from+","+to+")"
  }

  class ChangeOwnerTraverser(val oldowner: Symbol, val newowner: Symbol) extends Traverser {
    override def traverse(tree: Tree) {
      if ((tree.isDef || tree.isInstanceOf[Function]) &&
          tree.symbol != NoSymbol && tree.symbol.owner == oldowner)
        tree.symbol.owner = newowner;
      super.traverse(tree)
    }
  }

  final class TreeList {
    private var trees = List[Tree]()
    def append(t: Tree): TreeList = { trees = t :: trees; this }
    def append(ts: List[Tree]): TreeList = { trees = ts reverse_::: trees; this }
    def toList: List[Tree] = trees.reverse
  }

  object posAssigner extends Traverser {
    var pos: Position = _
    override def traverse(t: Tree) {
      if (t != EmptyTree && t.pos == NoPosition) {
        t.setPos(pos)
        super.traverse(t)
      }
    }
  }

  def atPos[T <: Tree](pos: Position)(tree: T): T = {
    posAssigner.pos = pos
    posAssigner.traverse(tree)
    tree
  }

  class ForeachTreeTraverser(f: Tree => Unit) extends Traverser {
    override def traverse(t: Tree) {
      f(t)
      super.traverse(t)
    }
  }

  class FilterTreeTraverser(p: Tree => Boolean) extends Traverser {
    val hits = new ListBuffer[Tree]
    override def traverse(t: Tree) {
      if (p(t)) hits += t
      super.traverse(t)
    }
  }

  class FindTreeTraverser(p: Tree => Boolean) extends Traverser {
    var result: Option[Tree] = None
    override def traverse(t: Tree) {
      if (result.isEmpty) {
        if (p(t)) result = Some(t)
        super.traverse(t)
      }
    }
  }

  object resetPos extends Traverser {
    override def traverse(t: Tree) {
      if (t != EmptyTree) t.setPos(NoPosition)
      super.traverse(t)
    }
  }


  /** resets symbol and tpe fields in a tree, @see ResetAttrsTraverse
   */
  def resetAllAttrs[A<:Tree](x:A): A = { new ResetAttrsTraverser().traverse(x); x }
  def resetLocalAttrs[A<:Tree](x:A): A = { new ResetLocalAttrsTraverser().traverse(x); x }
  
  /** A traverser which resets symbol and tpe fields of all nodes in a given tree
   *  except for (1) TypeTree nodes, whose <code>.tpe</code> field is kept and
   *  (2) if a <code>.symbol</code> field refers to a symbol which is defined
   *  outside the tree, it is also kept.
   *
   *  (bq:) This traverser has mutable state and should be discarded after use
   */
  private class ResetAttrsTraverser extends Traverser {
    protected def isLocal(sym: Symbol): Boolean = true
    protected def resetDef(tree: Tree) {
      tree.symbol = NoSymbol
    }
    override def traverse(tree: Tree): Unit = {
      tree match {
        case _: DefTree | Function(_, _) | Template(_, _, _) =>
          resetDef(tree)
        case _ =>
          if (tree.hasSymbol && isLocal(tree.symbol)) tree.symbol = NoSymbol
      }
      tree.resetType()
      super.traverse(tree)
    }
  }

  private class ResetLocalAttrsTraverser extends ResetAttrsTraverser {
    private val erasedSyms = new HashSet[Symbol](8)
    override protected def isLocal(sym: Symbol) = 
      erasedSyms contains sym
    override protected def resetDef(tree: Tree) {
      erasedSyms addEntry tree.symbol
      super.resetDef(tree)
    }
    override def traverse(tree: Tree): Unit = tree match {
      case Template(parents, self, body) =>
        for (stat <- body)
          if (stat.isDef) erasedSyms.addEntry(stat.symbol)
        super.traverse(tree)
      case _ =>
        super.traverse(tree)
    }
  }

  /* hook to memoize trees in IDE */
  trait TreeKind {
    def isType : Boolean
    def isTerm : Boolean
    def isDef : Boolean
    def hasSymbol : Boolean
    def isTop : Boolean
  }
}

