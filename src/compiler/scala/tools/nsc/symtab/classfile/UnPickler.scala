/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package symtab
package classfile

import java.io.IOException
import java.lang.{Float, Double}

import scala.tools.nsc.util.{Position, NoPosition}

import Flags._
import PickleFormat._
import collection.mutable.{HashMap, ListBuffer}
import annotation.switch

/** This abstract class implements ..
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class UnPickler {
  val global: Global
  import global._

  /** Unpickle symbol table information descending from a class and/or module root
   *  from an array of bytes.
   *  @param bytes      bytearray from which we unpickle
   *  @param offset     offset from which unpickling starts
   *  @param classroot  the top-level class which is unpickled, or NoSymbol if unapplicable
   *  @param moduleroot the top-level module which is unpickled, or NoSymbol if unapplicable
   *  @param filename   filename associated with bytearray, only used for error messages
   */
  def unpickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) {
    try {
      val p = if (currentRun.isDefined && 
                  currentRun.picklerPhase != NoPhase &&
                  phase.id > currentRun.picklerPhase.id) currentRun.picklerPhase
              else phase
      atPhase(p) {
        new UnPickle(bytes, offset, classRoot, moduleRoot, filename)
      }
    } catch {
      case ex: IOException =>
        throw ex
      case ex: Throwable =>
        /*if (settings.debug.value)*/ ex.printStackTrace()
        throw new RuntimeException("error reading Scala signature of "+filename+": "+ex.getMessage())
    }
  }

  private class UnPickle(bytes: Array[Byte], offset: Int, classRoot: Symbol, moduleRoot: Symbol, filename: String) extends PickleBuffer(bytes, offset, -1) {
    if (settings.debug.value) global.log("unpickle " + classRoot + " and " + moduleRoot)
    checkVersion(filename)

    /** A map from entry numbers to array offsets */
    private val index = createIndex

    /** A map from entry numbers to symbols, types, or annotations */
    private val entries = new Array[AnyRef](index.length)
    
    /** A map from symbols to their associated `decls' scopes */
    private val symScopes = new HashMap[Symbol, Scope]

    for (i <- 0 until index.length) {
      if (isSymbolEntry(i))
        at(i, readSymbol)
      else if (isSymbolAnnotationEntry(i))
        at(i, {() => readSymbolAnnotation(); null})
      else if (isChildrenEntry(i))
        at(i, {() => readChildren(); null})
    }

    if (settings.debug.value) global.log("unpickled " + classRoot + ":" + classRoot.rawInfo + ", " + moduleRoot + ":" + moduleRoot.rawInfo);//debug

    private def checkVersion(filename: String) {
      val major = readNat()
      val minor = readNat()
      if (major != MajorVersion || minor > MinorVersion)
        throw new IOException("Scala signature " + classRoot.name + 
                              " has wrong version\n expected: " + 
                              MajorVersion + "." + MinorVersion + 
                              "\n found: " + major + "." + minor +
                              " in "+filename)
    }

    /** The `decls' scope associated with given symbol */
    private def symScope(sym: Symbol) = symScopes.get(sym) match {
      case None => val s = new Scope; symScopes(sym) = s; s
      case Some(s) => s
    }

    /** Does entry represent an (internal) symbol */
    private def isSymbolEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      (firstSymTag <= tag && tag <= lastSymTag &&
       (tag != CLASSsym || !isRefinementSymbolEntry(i)))
    }

    /** Does entry represent an (internal or external) symbol */
    private def isSymbolRef(i: Int): Boolean = {
      val tag = bytes(index(i))
      (firstSymTag <= tag && tag <= lastExtSymTag)
    }

    /** Does entry represent a name? */
    private def isNameEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      tag == TERMname || tag == TYPEname
    }

    /** Does entry represent a symbol annotation? */
    private def isSymbolAnnotationEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      tag == SYMANNOT
    }

    /** Does the entry represent children of a symbol? */
    private def isChildrenEntry(i: Int): Boolean = {
      val tag = bytes(index(i)).toInt
      tag == CHILDREN
    }

    /** Does entry represent a refinement symbol? 
     *  pre: Entry is a class symbol
     */
    private def isRefinementSymbolEntry(i: Int): Boolean = {
      val savedIndex = readIndex
      readIndex = index(i)
      val tag = readByte().toInt
      assert(tag == CLASSsym)
      
      readNat(); // read length
      val result = readNameRef() == nme.REFINE_CLASS_NAME.toTypeName
      readIndex = savedIndex
      result
    }

    /** If entry at <code>i</code> is undefined, define it by performing
     *  operation <code>op</code> with <code>readIndex at start of i'th
     *  entry. Restore <code>readIndex</code> afterwards.
     */
    private def at[T <: AnyRef](i: Int, op: () => T): T = {
      var r = entries(i)
      if (r eq null) {
        val savedIndex = readIndex
        readIndex = index(i)
        r = op()
        assert(entries(i) eq null, entries(i))
        entries(i) = r
        readIndex = savedIndex
      }
      r.asInstanceOf[T]
    }

    /** Read a name */
    private def readName(): Name = {
      val tag = readByte()
      val len = readNat()
      tag match {
        case TERMname => newTermName(bytes, readIndex, len)
        case TYPEname => newTypeName(bytes, readIndex, len)
        case _ => errorBadSignature("bad name tag: " + tag)
      }
    }

    /** Read a symbol */
    private def readSymbol(): Symbol = {
      val tag = readByte()
      val end = readNat() + readIndex
      var sym: Symbol = NoSymbol
      tag match {
        case EXTref | EXTMODCLASSref =>
          val name = readNameRef()
          val owner = if (readIndex == end) definitions.RootClass else readSymbolRef()
          def fromName(name: Name) = 
            if (name.toTermName == nme.ROOT) definitions.RootClass
            else if (name == nme.ROOTPKG) definitions.RootPackage
            else if (tag == EXTref) owner.info.decl(name)
            else owner.info.decl(name).moduleClass
          sym = fromName(name)
          // If sym not found try with expanded name.
          // This can happen if references to private symbols are
          // read from outside; for instance when checking the children of a class
          // (see t1722)
          if (sym == NoSymbol) {
            sym = fromName(owner.expandedName(name))
          }

          // If the owner is overloaded (i.e. a method), it's not possible to select the
          // right member => return NoSymbol. This can only happen when unpickling a tree.
          // the "case Apply" in readTree() takes care of selecting the correct alternative
          //  after parsing the arguments.
          if (sym == NoSymbol && !owner.hasFlag(OVERLOADED)) {
            errorMissingRequirement(
              "reference " + (if (name.isTypeName) "type " else "value ") +
              name.decode + " of " + owner + " refers to nonexisting symbol.")
          }
        case NONEsym =>
          sym = NoSymbol
        case _ => // symbols that were pickled with Pickler.writeSymInfo
          var defaultGetter: Symbol = NoSymbol
          var nameref = readNat()
          if (tag == VALsym && isSymbolRef(nameref)) {
            defaultGetter = at(nameref, readSymbol)
            nameref = readNat()
          }
          val name = at(nameref, readName)
          val owner = readSymbolRef()
          val flags = pickledToRawFlags(readLongNat())
          var privateWithin: Symbol = NoSymbol
          var inforef = readNat()
          if (isSymbolRef(inforef)) {
            privateWithin = at(inforef, readSymbol)
            inforef = readNat()
          }
          tag match {
            case TYPEsym =>
              sym = owner.newAbstractType(NoPosition, name)
            case ALIASsym =>
              sym = owner.newAliasType(NoPosition, name)
            case CLASSsym =>
              sym = 
                if (name == classRoot.name && owner == classRoot.owner)
                  (if ((flags & MODULE) != 0L) moduleRoot.moduleClass
                   else classRoot)
                else 
                  if ((flags & MODULE) != 0L) owner.newModuleClass(NoPosition, name)
                  else owner.newClass(NoPosition, name)
              if (readIndex != end) sym.typeOfThis = new LazyTypeRef(readNat())
            case MODULEsym =>
              val clazz = at(inforef, readType).typeSymbol
              sym = 
                if (name == moduleRoot.name && owner == moduleRoot.owner) moduleRoot
                else {
                  assert(clazz.isInstanceOf[ModuleClassSymbol], clazz)
                  val mclazz = clazz.asInstanceOf[ModuleClassSymbol]
                  val m = owner.newModule(NoPosition, name, mclazz)
                  mclazz.setSourceModule(m)
                  m
                }
            case VALsym =>
              sym = if (name == moduleRoot.name && owner == moduleRoot.owner) moduleRoot.resetFlag(MODULE)
                    else owner.newValue(NoPosition, name)
              sym.defaultGetter = defaultGetter
            case _ =>
              errorBadSignature("bad symbol tag: " + tag)
          }
          sym.setFlag(flags.toLong & PickledFlags)
          sym.privateWithin = privateWithin
          if (readIndex != end) assert(sym hasFlag (SUPERACCESSOR | PARAMACCESSOR), sym)
          if (sym hasFlag SUPERACCESSOR) assert(readIndex != end)
          sym.setInfo(
            if (readIndex != end) new LazyTypeRefAndAlias(inforef, readNat())
            else new LazyTypeRef(inforef))
          if (sym.owner.isClass && sym != classRoot && sym != moduleRoot && 
              !sym.isModuleClass && !sym.isRefinementClass && !sym.isTypeParameter && !sym.isExistential)
            symScope(sym.owner) enter sym
      }
      sym
    }

    /** Read a type */
    private def readType(): Type = {
      val tag = readByte()
      val end = readNat() + readIndex
      tag match {
        case NOtpe =>
          NoType
        case NOPREFIXtpe =>
          NoPrefix
        case THIStpe =>
          mkThisType(readSymbolRef())
        case SINGLEtpe =>
          singleType(readTypeRef(), readSymbolRef())
        case SUPERtpe =>
          val thistpe = readTypeRef()
          val supertpe = readTypeRef()
          SuperType(thistpe, supertpe)
        case CONSTANTtpe =>
          mkConstantType(readConstantRef())
        case TYPEREFtpe =>
          val pre = readTypeRef()
          val sym = readSymbolRef()
          var args = until(end, readTypeRef)
          rawTypeRef(pre, sym, args)
        case TYPEBOUNDStpe =>
          mkTypeBounds(readTypeRef(), readTypeRef())
        case REFINEDtpe =>
          val clazz = readSymbolRef()
/*
          val ps = until(end, readTypeRef)
          val dcls = symScope(clazz)
          new RefinedType(ps, dcls) { override def symbol = clazz }
*/
         new RefinedType(until(end, readTypeRef), symScope(clazz)) {
           override def typeSymbol = clazz
          }
        case CLASSINFOtpe =>
          val clazz = readSymbolRef()
          ClassInfoType(until(end, readTypeRef), symScope(clazz), clazz)
        case METHODtpe =>
          val restpe = readTypeRef()
          val params = until(end, readSymbolRef)
          // if the method is overloaded, the params cannot be determined (see readSymbol) => return NoType.
          // Only happen for trees, "case Apply" in readTree() takes care of selecting the correct
          // alternative after parsing the arguments.
          if (params.contains(NoSymbol) || restpe == NoType) NoType
          else MethodType(params, restpe)
        case IMPLICITMETHODtpe =>
          val restpe = readTypeRef()
          val params = until(end, readSymbolRef)
          ImplicitMethodType(params, restpe)
        case POLYtpe =>
          val restpe = readTypeRef()
          val typeParams = until(end, readSymbolRef)
          // see comment above in "case METHODtpe"
          if (typeParams.contains(NoSymbol) || restpe == NoType) NoType
          else PolyType(typeParams, restpe)
        case EXISTENTIALtpe =>
          val restpe = readTypeRef()
          ExistentialType(until(end, readSymbolRef), restpe)
        case ANNOTATEDtpe =>
          var typeRef = readNat()
          val selfsym = if (isSymbolRef(typeRef)) {
            val s = at(typeRef, readSymbol)
            typeRef = readNat()
            s
          } else NoSymbol
          val tp = at(typeRef, readType)
          val annots = until(end, readAnnotationRef)
          if (settings.selfInAnnots.value || (selfsym == NoSymbol))
            AnnotatedType(annots, tp, selfsym)
          else
            tp // drop annotations with a self symbol unless
               // -Yself-in-annots is on
        case DEBRUIJNINDEXtpe =>
          DeBruijnIndex(readNat(), readNat())
        case _ =>
          errorBadSignature("bad type tag: " + tag)
      }
    }

    /** Read a constant */
    private def readConstant(): Constant = {
      val tag = readByte().toInt
      val len = readNat()
      (tag: @switch) match {
        case LITERALunit    => Constant(())
        case LITERALboolean => Constant(readLong(len) != 0L)
        case LITERALbyte    => Constant(readLong(len).toByte)
        case LITERALshort   => Constant(readLong(len).toShort)
        case LITERALchar    => Constant(readLong(len).toChar)
        case LITERALint     => Constant(readLong(len).toInt)
        case LITERALlong    => Constant(readLong(len))
        case LITERALfloat   => Constant(Float.intBitsToFloat(readLong(len).toInt))
        case LITERALdouble  => Constant(Double.longBitsToDouble(readLong(len)))
        case LITERALstring  => Constant(readNameRef().toString())
        case LITERALnull    => Constant(null)
        case LITERALclass   => Constant(readTypeRef())
        case LITERALenum    => Constant(readSymbolRef())
        case _              => errorBadSignature("bad constant tag: " + tag)
      }
    }

    /** Read children and store them into the corresponding symbol.
     */
    private def readChildren() {
      val tag = readByte()
      assert(tag == CHILDREN)
      val end = readNat() + readIndex
      val target = readSymbolRef()
      while (readIndex != end) target addChild readSymbolRef()
    }

    /** Read an annotation argument, which is pickled either
     *  as a Constant or a Tree.
     */
    private def readAnnotArg(): Tree = {
      if (peekByte() == TREE) {
        readTree()
      } else {
        val const = readConstant()
        Literal(const).setType(const.tpe)
      }
    }

    /** Read a ClassfileAnnotArg (argument to a classfile annotation)
     */
    private def readClassfileAnnotArg(): ClassfileAnnotArg = peekByte() match {
      case ANNOTINFO      =>
        NestedAnnotArg(readAnnotation())
      case ANNOTARGARRAY  =>
        readByte()
        val end = readNat() + readIndex
        ArrayAnnotArg(until(end, readClassfileAnnotArgRef).toArray)
      case _              =>
        LiteralAnnotArg(readConstant())
    }

    /** Read an AnnotationInfo. Not to be called directly, use
     *  readAnnotation or readSymbolAnnotation
     */
    private def readAnnotationInfo(end: Int): AnnotationInfo = {
      val atp = readTypeRef()
      val args = new ListBuffer[Tree]
      val assocs = new ListBuffer[(Name, ClassfileAnnotArg)]
      while (readIndex != end) {
        val argref = readNat()
        if (isNameEntry(argref))
          assocs += ((at(argref, readName), readClassfileAnnotArgRef))
        else
          args += at(argref, readAnnotArg)
      }
      AnnotationInfo(atp, args.toList, assocs.toList)
    }

    /** Read an annotation and as a side effect store it into
     *  the symbol it requests. Called at top-level, for all
     *  (symbol, annotInfo) entries. */
    private def readSymbolAnnotation() {
      val tag = readByte()
      if (tag != SYMANNOT)
        errorBadSignature("symbol annotation expected ("+ tag +")")
      val end = readNat() + readIndex
      val target = readSymbolRef()
      target.addAnnotation(readAnnotationInfo(end))
    }

    /** Read an annotation and return it. Used when unpickling
     *  an ANNOTATED(WSELF)tpe or a NestedAnnotArg */
    private def readAnnotation(): AnnotationInfo = {
      val tag = readByte()
      if (tag != ANNOTINFO)
        errorBadSignature("annotation expected (" + tag + ")")
      val end = readNat() + readIndex
      readAnnotationInfo(end)
    }

    /* Read an abstract syntax tree */
    private def readTree(): Tree = {
      val outerTag = readByte()
      if (outerTag != TREE)
        errorBadSignature("tree expected (" + outerTag + ")")
      val end = readNat() + readIndex
      val tag = readByte()
      val tpe = if (tag == EMPTYtree) NoType else readTypeRef()
      
      // Set by the three functions to follow.  If symbol is non-null
      // after the the new tree 't' has been created, t has its Symbol
      // set to symbol; and it always has its Type set to tpe.
      var symbol: Symbol = null
      var mods: Modifiers = null
      var name: Name = null

      /** Read a Symbol, Modifiers, and a Name */
      def setSymModsName() {
        symbol = readSymbolRef()
        mods = readModifiersRef()
        name = readNameRef()
      }
      /** Read a Symbol and a Name */
      def setSymName() {
        symbol = readSymbolRef()
        name = readNameRef()
      }
      /** Read a Symbol */
      def setSym() {
        symbol = readSymbolRef()
      }

      val t = tag match {
        case EMPTYtree =>
          EmptyTree

        case PACKAGEtree =>
          setSym()
          // val discardedSymbol = readSymbolRef()  // XXX is symbol intentionally not set?
          val pid = readTreeRef().asInstanceOf[RefTree]
          val stats = until(end, readTreeRef)
          PackageDef(pid, stats)

        case CLASStree =>
          setSymModsName()
          val impl = readTemplateRef()
          val tparams = until(end, readTypeDefRef)
          ClassDef(mods, name, tparams, impl)

        case MODULEtree =>
          setSymModsName()
          ModuleDef(mods, name, readTemplateRef())

        case VALDEFtree =>
          setSymModsName()
          val tpt = readTreeRef()
          val rhs = readTreeRef()
          ValDef(mods, name, tpt, rhs)

        case DEFDEFtree =>
          setSymModsName()
          val tparams = times(readNat(), readTypeDefRef)
          val vparamss = times(readNat(), () => times(readNat(), readValDefRef))
          val tpt = readTreeRef()
          val rhs = readTreeRef()
          
          DefDef(mods, name, tparams, vparamss, tpt, rhs)

        case TYPEDEFtree =>
          setSymModsName()
          val rhs = readTreeRef()
          val tparams = until(end, readTypeDefRef)
          TypeDef(mods, name, tparams, rhs)

        case LABELtree =>
          setSymName()
          val rhs = readTreeRef()
          val params = until(end, readIdentRef)
          LabelDef(name, params, rhs)

        case IMPORTtree =>
          setSym()
          val expr = readTreeRef()
          val selectors = until(end, () => {
            val from = readNameRef()
            val to = readNameRef()
            ImportSelector(from, -1, to, -1)
          })

          Import(expr, selectors)

        case DOCDEFtree =>
          val comment = readConstantRef match {
            case Constant(com: String)  => com
            case other => errorBadSignature("Document comment not a string (" + other + ")")
          }
          val definition = readTreeRef()
          DocDef(comment, definition)

        case TEMPLATEtree =>
          setSym()
          val parents = times(readNat(), readTreeRef)
          val self = readValDefRef()
          val body = until(end, readTreeRef)

          Template(parents, self, body)

        case BLOCKtree =>
          val expr = readTreeRef()
          val stats = until(end, readTreeRef)
          Block(stats, expr)

        case CASEtree =>
          val pat = readTreeRef()
          val guard = readTreeRef()
          val body = readTreeRef()
          CaseDef(pat, guard, body)

        case ALTERNATIVEtree =>
          Alternative(until(end, readTreeRef))

        case STARtree =>
          Star(readTreeRef())

        case BINDtree =>
          setSymName()
          Bind(name, readTreeRef())

        case UNAPPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          UnApply(fun, args)

        case ARRAYVALUEtree =>
          val elemtpt = readTreeRef()
          val trees = until(end, readTreeRef)
          ArrayValue(elemtpt, trees)

        case FUNCTIONtree =>
          setSym()
          val body = readTreeRef()
          val vparams = until(end, readValDefRef)
          Function(vparams, body)

        case ASSIGNtree =>
          val lhs = readTreeRef()
          val rhs = readTreeRef()
          Assign(lhs, rhs)

        case IFtree =>
          val cond = readTreeRef()
          val thenp = readTreeRef()
          val elsep = readTreeRef()
          If(cond, thenp, elsep)

        case MATCHtree =>
          val selector = readTreeRef()
          val cases = until(end, readCaseDefRef)
          Match(selector, cases)

        case RETURNtree =>
          setSym()
          Return(readTreeRef())

        case TREtree =>
          val block = readTreeRef()
          val finalizer = readTreeRef()
          val catches = until(end, readCaseDefRef)
          Try(block, catches, finalizer)

        case THROWtree =>
          Throw(readTreeRef())

        case NEWtree =>
          New(readTreeRef())

        case TYPEDtree =>
          val expr = readTreeRef()
          val tpt = readTreeRef()
          Typed(expr, tpt)

        case TYPEAPPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          TypeApply(fun, args)

        case APPLYtree =>
          val fun = readTreeRef()
          val args = until(end, readTreeRef)
          if (fun.symbol hasFlag OVERLOADED) {
            fun.setType(fun.symbol.info)
            typer.infer.inferMethodAlternative(fun, Nil, args map (_.tpe), tpe)
          }
          Apply(fun, args)

        case APPLYDYNAMICtree =>
          setSym()
          val qual = readTreeRef()
          val args = until(end, readTreeRef)
          ApplyDynamic(qual, args)

        case SUPERtree =>
          setSym()
          val qual = readNameRef()
          val mix = readNameRef()
          Super(qual, mix)

        case THIStree =>
          setSym()
          This(readNameRef())

        case SELECTtree =>
          setSym()
          val qualifier = readTreeRef()
          val selector = readNameRef()
          Select(qualifier, selector)

        case IDENTtree =>
          setSymName()
          Ident(name)

        case LITERALtree =>
          Literal(readConstantRef())

        case TYPEtree =>
          TypeTree()

        case ANNOTATEDtree =>
          val annot = readTreeRef()
          val arg = readTreeRef()
          Annotated(annot, arg)

        case SINGLETONTYPEtree =>
          SingletonTypeTree(readTreeRef())

        case SELECTFROMTYPEtree =>
          val qualifier = readTreeRef()
          val selector = readNameRef()
          SelectFromTypeTree(qualifier, selector)

        case COMPOUNDTYPEtree =>
          CompoundTypeTree(readTemplateRef())

        case APPLIEDTYPEtree =>
          val tpt = readTreeRef()
          val args = until(end, readTreeRef)
          AppliedTypeTree(tpt, args)

        case TYPEBOUNDStree =>
          val lo = readTreeRef()
          val hi = readTreeRef()
          TypeBoundsTree(lo, hi)

        case EXISTENTIALTYPEtree =>
          val tpt = readTreeRef()
          val whereClauses = until(end, readTreeRef)
          ExistentialTypeTree(tpt, whereClauses)

        case _ =>
          errorBadSignature("unknown tree type (" + tag + ")")
      }

      if (symbol == null) t setType tpe
      else t setSymbol symbol setType tpe      
    }

    def readModifiers(): Modifiers = {
      val tag = readNat()
      if (tag != MODIFIERS)
        errorBadSignature("expected a modifiers tag (" + tag + ")")
      val end = readNat() + readIndex
      val pflagsHi = readNat()
      val pflagsLo = readNat()
      val pflags = (pflagsHi.toLong << 32) + pflagsLo
      val flags = pickledToRawFlags(pflags)
      val privateWithin = readNameRef()
      Modifiers(flags, privateWithin, Nil, new Map.EmptyMap)
    }

    /* Read a reference to a pickled item */
    private def readNameRef(): Name = at(readNat(), readName)
    private def readSymbolRef(): Symbol = at(readNat(), readSymbol)
    private def readTypeRef(): Type = at(readNat(), readType)
    private def readConstantRef(): Constant = at(readNat(), readConstant)
    private def readAnnotArgRef(): Tree =
      at(readNat(), readAnnotArg)
    private def readClassfileAnnotArgRef(): ClassfileAnnotArg =
      at(readNat(), readClassfileAnnotArg)
    private def readAnnotationRef(): AnnotationInfo =
      at(readNat(), readAnnotation)
    private def readModifiersRef(): Modifiers =
      at(readNat(), readModifiers)
    private def readTreeRef(): Tree =
      at(readNat(), readTree)

    private def readTemplateRef(): Template =
      readTreeRef() match {
        case templ:Template => templ
        case other =>
          errorBadSignature("expected a template (" + other + ")")
      }
    private def readCaseDefRef(): CaseDef =
      readTreeRef() match {
        case tree:CaseDef => tree
        case other =>
          errorBadSignature("expected a case def (" + other + ")")
      }
    private def readValDefRef(): ValDef =
      readTreeRef() match {
        case tree:ValDef => tree
        case other =>
          errorBadSignature("expected a ValDef (" + other + ")")
      }
    private def readIdentRef(): Ident =
      readTreeRef() match {
        case tree:Ident => tree
        case other =>
          errorBadSignature("expected an Ident (" + other + ")")
      }
    private def readTypeDefRef(): TypeDef =
      readTreeRef() match {
        case tree:TypeDef => tree
        case other =>
          errorBadSignature("expected an TypeDef (" + other + ")")
      }

    private def errorBadSignature(msg: String) =
      throw new RuntimeException("malformed Scala signature of " + classRoot.name + " at " + readIndex + "; " + msg)

    private def errorMissingRequirement(msg: String) =
      if (settings.debug.value) errorBadSignature(msg)
      else throw new IOException("class file needed by "+classRoot.name+" is missing.\n"+msg) 

    private class LazyTypeRef(i: Int) extends LazyType {
      private val definedAtRunId = currentRunId
      private val p = phase
      override def complete(sym: Symbol) : Unit = {
        val tp = at(i, readType)
        if (p != phase) atPhase(p) (sym setInfo tp) 
        else sym setInfo tp
        if (currentRunId != definedAtRunId) sym.setInfo(adaptToNewRunMap(tp))
      }
      override def load(sym: Symbol) { complete(sym) }
    }

    private class LazyTypeRefAndAlias(i: Int, j: Int) extends LazyTypeRef(i) {
      override def complete(sym: Symbol) {
        super.complete(sym)
        var alias = at(j, readSymbol)
        if (alias hasFlag OVERLOADED) {
          atPhase(currentRun.picklerPhase) {
            alias = alias suchThat (alt => sym.tpe =:= sym.owner.thisType.memberType(alt))
          }
        }
        sym.asInstanceOf[TermSymbol].setAlias(alias)
      }
    }
  }
}
