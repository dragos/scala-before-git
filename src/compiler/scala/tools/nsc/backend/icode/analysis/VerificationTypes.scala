/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */

// $Id$

package scala.tools.nsc
package backend.icode.analysis

import scala.collection.{mutable, immutable}

/** Infer types at the beginning of each basic block, to be
 *  used for generating JVM 1.6 bytecodes that use the type-checking
 *  verifier.
 *
 *  @author Iulian Dragos
 */
abstract class VerificationTypes {
  val global: Global
  import global._
  import icodes._

  /** The lattice of ICode types.
   */
  object verificationTypeLattice extends CompleteLattice {
    type Elem = VerificationType

    val Object = icodes.REFERENCE(global.definitions.ObjectClass)
    val All    = icodes.REFERENCE(global.definitions.NothingClass)

    abstract class VerificationType {
      def getSignature: String = this match {
        case Bool => "Z"
        case Byte => "B"
        case Char => "C"
        case Short => "S"
        case Int => "I"
        case Long => "J"
        case Float => "F"
        case Double => "D"
        case ReferenceType(cls) =>
          global.genJVM.javaName(cls)
        case ArrayOf(tp) => "[" + tp.getDescriptor
        case _ => Predef.error("unknown signature for " + this)
      }

      def getDescriptor: String = this match {
        case ReferenceType(cls) => "L" + getSignature + ";"
        case _ => getSignature
      }
    }

    case object Top extends VerificationType
    case object Bottom extends VerificationType
    case object Int extends VerificationType
    case object Float extends VerificationType
    case object Long extends VerificationType
    case object Double extends VerificationType
    case class  Uninitialized(pos: (BasicBlock, Int)) extends VerificationType
    case object UninitializedThis extends VerificationType
    case class  ReferenceType(cls: Symbol) extends VerificationType {
      assert(cls != definitions.ArrayClass)
      assert(!cls.isRefinementClass)
    }
    case class  ArrayOf(v: VerificationType) extends VerificationType
    case object NullType extends VerificationType
    // the following types are used only as component type of an array type
    case object Bool extends VerificationType
    case object Byte extends VerificationType
    case object Char extends VerificationType
    case object Short extends VerificationType

    val AnyRefType = ReferenceType(definitions.AnyRefClass)
    val StringBufferType = ReferenceType(definitions.getClass("scala.collection.mutable.StringBuilder"))

    /** A map from scala primitive Types to ICode TypeKinds */
    lazy val primitiveTypeMap: collection.Map[Symbol, VerificationType] = {
      import definitions._
      collection.Map(
        UnitClass     -> Top,
        BooleanClass  -> Int,
        CharClass     -> Int,
        ByteClass     -> Int,
        ShortClass    -> Int,
        IntClass      -> Int,
        LongClass     -> Long,
        FloatClass    -> Float,
        DoubleClass   -> Double
      )
    }

    def top    = Top
    def bottom = Bottom

    /** The least upper bound of two types.  */
    def lub2(exceptional: Boolean, old: Elem)(a: Elem, b: Elem): Elem =
      if (a == b) a else (a, b) match {
        case (Bottom, _) => b
        case (_, Bottom) => a
        case (Top, _) | (_, Top) => Top
        case (ReferenceType(cls1), ReferenceType(cls2)) =>
          def bestLub: Symbol = {
            val lub0: Type = atPhase(global.currentRun.refchecksPhase)(global.lub(cls1.tpe :: cls2.tpe :: Nil))
            def firstNonTraitParent(tpe: Type): Symbol = tpe match {
              case RefinedType(parents, decls) =>
                parents.find(!_.typeSymbol.isTrait).getOrElse(parents.head).typeSymbol
              case NotNullType(underlying) => firstNonTraitParent(underlying)
              case _ => tpe.typeSymbol
            }
            firstNonTraitParent(lub0)
          }

          println("lubbing " + cls1  + " and " + cls2 + " => " +
                  atPhase(global.currentRun.refchecksPhase)(global.lub(cls1.tpe :: cls2.tpe :: Nil))
                  + " got back " + bestLub)
          ReferenceType(bestLub)
        case (Uninitialized(p1), Uninitialized(p2)) if p1 == p2 => a
        case (NullType, ReferenceType(_)) => b
        case (ReferenceType(_), NullType) => a
        case (ArrayOf(e1), ArrayOf(e2)) =>
          lub2(exceptional, old)(e1, e2) match {
            case Top => AnyRefType
            case e => ArrayOf(e)
          }
        case (ArrayOf(_), ReferenceType(_)) => AnyRefType
        case (ReferenceType(_), ArrayOf(_)) => AnyRefType
        case (ArrayOf(_), NullType) => a
        case (NullType, ArrayOf(_)) => b
        case (ReferenceType(_), NullType) => a
        case (NullType, ReferenceType(_)) => b
        case _ =>
          println("emitting Top lub of " + a + ", " + b)
          Top
      }

    implicit def verificationType(tp: TypeKind): VerificationType = tp match {
      case BOOL | BYTE | SHORT | CHAR | INT => Int
      case LONG => Long
      case FLOAT => Float
      case DOUBLE => Double
      case REFERENCE(cls) => ReferenceType(cls)
      case ARRAY(e) =>
        e match {
          case BOOL => ArrayOf(Bool)
          case BYTE => ArrayOf(Byte)
          case CHAR => ArrayOf(Char)
          case SHORT => ArrayOf(Short)
          case _ => ArrayOf(verificationType(e))
        }
      case ConcatClass => StringBufferType
      case UNIT => Predef.error("found unit")
      case BOXED(kind) => (kind: @unchecked) match {
        case BOOL => ReferenceType(definitions.BoxedBooleanClass)
        case BYTE => ReferenceType(definitions.BoxedByteClass)
        case SHORT => ReferenceType(definitions.BoxedShortClass)
        case CHAR => ReferenceType(definitions.BoxedCharacterClass)
        case INT => ReferenceType(definitions.BoxedIntClass)
        case LONG => ReferenceType(definitions.BoxedLongClass)
        case FLOAT => ReferenceType(definitions.BoxedFloatClass)
        case DOUBLE => ReferenceType(definitions.BoxedDoubleClass)
      }
    }

    def verificationType(tpe: Type): VerificationType =
      verificationType(toTypeKind(tpe))
  }

  /** The lattice of type stacks. It is a straight forward extension of
   *  the type lattice (lub is pairwise lub of the list elements).
   */
  object typeStackLattice extends CompleteLattice {
    import verificationTypeLattice.{VerificationType, ReferenceType}
    type Elem = TypeStack[VerificationType]

    override val top: Elem    = new TypeStack[VerificationType]
    override val bottom: Elem = new TypeStack[VerificationType]

    def exceptionHandlerStack(target: Symbol) = {
      val s = new TypeStack[VerificationType]
      s.push(ReferenceType(if (target == NoSymbol) definitions.ThrowableClass else target))
    }

    def lub2(exceptional: Boolean, old: Elem)(s1: Elem, s2: Elem): TypeStack[VerificationType] = {
      if (s1 eq bottom) s2
      else if (s2 eq bottom) s1
      else if (exceptional) {
        println("exceptional handler, resuing old value: " + old)
        old
      }
      else {
        val tps = (s1.types, s2.types, old.types).zipped map { (t1, t2, old) =>
          verificationTypeLattice.lub2(exceptional, old)(t1, t2)
        }
        (new TypeStack[VerificationType]).pushAll(tps.reverse)
      }
    }
  }

  object varBindingLattice extends CompleteLattice {
    import verificationTypeLattice.{VerificationType, ReferenceType}
    type VarBinding = mutable.LinkedHashMap[icodes.Local, verificationTypeLattice.Elem]
    type Elem = VarBinding

    val top = new VarBinding
    val bottom = new VarBinding

    def lub2(exceptional: Boolean, old: Elem)(a: VarBinding, b: VarBinding): VarBinding = {
      if (a eq bottom) b
      else if (b eq bottom) a
      else {
        val resultingLocals = new VarBinding

        for (binding1 <- a.iterator) {
          b.get(binding1._1) match {
            case Some(tp2) =>
              resultingLocals += ((binding1._1, verificationTypeLattice.lub2(exceptional, tp2)(binding1._2, tp2)))
            case None =>
//              resultingLocals += ((binding1._1, verificationTypeLattice.bottom))
          }
        }
        resultingLocals
      }
    }
  }



  /** The type flow lattice contains a binding from local variable
   *  names to types and a type stack.
   */
  object typeFlowLattice extends CompleteLattice {
    import icodes._
    import varBindingLattice._

    type Elem = IState[VarBinding, typeStackLattice.Elem]

    override val top    = new Elem(varBindingLattice.top, typeStackLattice.top)
    override val bottom = new Elem(varBindingLattice.bottom, typeStackLattice.bottom)

    // a dummy local variable representing bindings of 'this'
    val thisLocal = new icodes.Local(NoSymbol, null, false)
    thisLocal.index = 0

    def lub2(exceptional: Boolean, old: Elem)(a: Elem, b: Elem) = {
      val IState(env1, s1) = a
      val IState(env2, s2) = b

      lazy val resultingBindings = varBindingLattice.lub2(exceptional, old.vars)(env1, env2)
      if (exceptional) IState(resultingBindings, old.stack)
      else if (a eq bottom) b
      else if (b eq bottom) a
      else IState(resultingBindings, typeStackLattice.lub2(exceptional, old.stack)(a.stack, b.stack))
    }
  }

  val timer = new Timer

  class InferTypes extends DataFlowAnalysis[typeFlowLattice.type] {
    import verificationTypeLattice._
    import icodes._
    import icodes.opcodes._

    type P = BasicBlock
    val lattice = typeFlowLattice

    val STRING = icodes.REFERENCE(VerificationTypes.this.global.definitions.StringClass)
    var method: IMethod = _

    /** Initialize the in/out maps for the analysis of the given method. */
    def init(m: icodes.IMethod) {
      this.method = m
      //typeFlowLattice.lubs = 0
      init {
        worklist += m.code.startBlock
//        worklist ++= (m.exh map (_.startBlock))
        m.code.blocks.foreach { b =>
          in(b)  = typeFlowLattice.bottom
          out(b) = typeFlowLattice.bottom
        }

        // start block has var bindings for each of its parameters
        val entryBindings = new varBindingLattice.VarBinding
        m.params.foreach(p => entryBindings += ((p, verificationType(p.kind))))
        if (!m.isStatic)
          entryBindings += ((typeFlowLattice.thisLocal,
            if (m.symbol.isClassConstructor) UninitializedThis else verificationType(m.symbol.owner.tpe)))

        in(m.code.startBlock) = lattice.IState(entryBindings, typeStackLattice.bottom)

        m.exh foreach { e =>
          in(e.startBlock) = lattice.IState(varBindingLattice.bottom, typeStackLattice.exceptionHandlerStack(e.cls))
        }
      }
    }

    def this(m: icodes.IMethod) {
      this()
      init(m)
    }

    def run = {
      timer.start
//      icodes.lubs0 = 0
      forwardAnalysis(blockTransfer)
      val t = timer.stop
      if (settings.debug.value) {
        linearizer.linearize(method).foreach(b => if (b != method.code.startBlock)
          assert(visited.contains(b),
            "Block " + b + " in " + this.method + " has input equal to bottom -- not visited? .." + visited));
      }
//      log("" + method.symbol.fullNameString + " ["  + method.code.blocks.size + " blocks] "
//              + "\n\t" + iterations + " iterations: " + t + " ms."
//              + "\n\tlubs: " + typeFlowLattice.lubs + " out of which " + icodes.lubs0 + " typer lubs")
    }

    def blockTransfer(b: BasicBlock, in: lattice.Elem): lattice.Elem = {
      /** Abstract interpretation for one instruction. */      
      def interpret(in: typeFlowLattice.Elem, instr: (Instruction, Int)): typeFlowLattice.Elem = {
        import verificationTypeLattice.verificationType
        def pop2(st: mutable.Stack[VerificationType]) = (st.pop, st.pop)
        def pop(st: mutable.Stack[VerificationType], n: Int) = {
          var m = n
          while (m > 0) { st.pop; m -= 1 }
        }
        
        val (i, idx) = instr

        val out = lattice.IState(in.vars.clone, new TypeStack(in.stack))
        val bindings = out.vars
        val stack = out.stack
        def replaceUninitialized(instance: VerificationType, result: VerificationType) {
          // replace its occurences by the proper reference type
          stack.map(t => if (t == instance) result else t)
          bindings.foreach { case (k, v) => bindings.update(k, if (v == instance) result else v) }
        }


        if (settings.debug.value) {
          Console.println("[before] locals: " + bindings)
          Console.println("[before] Stack: " + stack);      
          Console.println(i);
        }
        i match {

          case THIS(clasz) =>
            stack push bindings.get(typeFlowLattice.thisLocal).getOrElse(verificationType(clasz.tpe))

          case CONSTANT(const) =>
            if (const.tag == NullTag)
              stack.push(NullType)
            else
              stack.push(verificationType(const.tpe))

          case LOAD_ARRAY_ITEM(kind) =>
            stack.pop2
            stack.push(verificationType(kind))

          case LOAD_LOCAL(local) =>
            bindings.get(local) match {     
              case Some(t) => stack.push(t)
              case None => stack.push(verificationTypeLattice.bottom)
            }

          case LOAD_FIELD(field, isStatic) =>
            if (!isStatic) stack.pop
            stack.push(verificationType(field.tpe))

          case LOAD_MODULE(module) =>
            stack.push(verificationType(module.tpe))

          case STORE_ARRAY_ITEM(kind) =>
            stack.pop(3)

          case STORE_LOCAL(local) =>
            val t = stack.pop
            bindings += (local -> t)

          case STORE_THIS(_) =>
            stack.pop

          case STORE_FIELD(field, isStatic) =>
            if (isStatic)
              stack.pop
            else
              stack.pop2

          case CALL_PRIMITIVE(primitive) =>
            primitive match {
              case Negation(kind) =>
                stack.pop; stack.push(kind)
              case Test(_, kind, zero) =>
                stack.pop
                if (!zero) stack.pop
                stack push Int;
              case Comparison(_, _) =>
                stack.pop2
                stack push Int

              case Arithmetic(op, kind) =>
                stack.pop
                if (op != NOT)
                  stack.pop
                val k = kind match {
                  case BYTE | SHORT | CHAR => Int
                  case _ => verificationType(kind)
                }
                stack push k

              case Logical(op, kind) =>
                stack.pop2
                stack push kind

              case Shift(op, kind) =>
                stack.pop2
                stack push kind

              case Conversion(src, dst) =>
                stack.pop
                stack push dst

              case ArrayLength(kind) =>
                stack.pop
                stack push Int

              case StartConcat =>
                stack.push(ConcatClass)

              case EndConcat =>
                stack.pop
                stack.push(STRING)

              case StringConcat(el) =>
                stack.pop2
                stack push ConcatClass
            }

          case CALL_METHOD(method, style) => style match {
            case Dynamic | InvokeDynamic =>
              stack.pop(1 + method.info.paramTypes.length)
              if (method.info.resultType.typeSymbol != definitions.UnitClass)
                stack.push(verificationType(method.info.resultType))

            case Static(onInstance) =>
              if (onInstance) {
                stack.pop(method.info.paramTypes.length)
                val uninit = stack.pop // (maybe uninitialized) instance
                if (method.info.resultType.typeSymbol != definitions.UnitClass) {
                  val result = verificationType(toTypeKind(method.info.resultType))
                  if (method.isClassConstructor) {
                    replaceUninitialized(uninit, result)
                  } else
                    stack.push(result)
                }
              } else {
                stack.pop(method.info.paramTypes.length)
                if (method.info.resultType.typeSymbol != definitions.UnitClass)
                  stack.push(toTypeKind(method.info.resultType))
              }

            case SuperCall(mix) =>
              stack.pop(method.info.paramTypes.length)
              val receiver = stack.pop
              if (method.isClassConstructor) {
                assert(receiver == UninitializedThis)
                replaceUninitialized(receiver, ReferenceType(this.method.symbol.owner)) // current class
              } else if (method.info.resultType.typeSymbol != definitions.UnitClass)
                stack.push(toTypeKind(method.info.resultType))
          }

          case BOX(kind) =>
            stack.pop
            stack.push(AnyRefType)

          case UNBOX(kind) =>
            stack.pop
            stack.push(kind)

          case NEW(kind) =>
            stack.push(Uninitialized((b, idx)))

          case CREATE_ARRAY(elem, dims) =>
            stack.pop(dims)
            stack.push(verificationType(ARRAY(elem)))

          case IS_INSTANCE(tpe) =>
            stack.pop
            stack.push(Int)

          case CHECK_CAST(tpe) =>
            stack.pop
            stack.push(verificationType(tpe))

          case SWITCH(tags, labels) =>
            stack.pop

          case JUMP(whereto) =>
            ()

          case CJUMP(success, failure, cond, kind) =>
            stack.pop(2)

          case CZJUMP(success, failure, cond, kind) =>
            stack.pop

          case RETURN(kind) =>
            if (kind != UNIT)
              stack.pop;

          case THROW() =>
            stack.pop

          case DROP(kind) =>
            stack.pop

          case DUP(kind) =>
            stack.push(stack.head)

          case MONITOR_ENTER() =>
            stack.pop

          case MONITOR_EXIT() =>
            stack.pop

          case SCOPE_ENTER(_) =>
            ()

          case SCOPE_EXIT(l) =>
            bindings.removeKey(l)

          case LOAD_EXCEPTION() =>
            stack.clear
            stack.push(AnyRefType)

          case _ =>
            dump
            abort("Unknown instruction: " + i)

        }
        out
      } // interpret

      b.zipWithIndex.foldLeft(in)(interpret)
    }
  }

  class Timer {
    var millis = 0L

    private var lastStart = 0L

    def reset {
      millis = 0L
    }

    def start {
      lastStart = System.currentTimeMillis
    }

    /** Stop the timer and return the number of milliseconds since the last
     * call to start. The 'millis' field is increased by the elapsed time.
     */
    def stop: Long = {
      val elapsed = System.currentTimeMillis - lastStart
      millis += elapsed
      elapsed
    }
  }
}
