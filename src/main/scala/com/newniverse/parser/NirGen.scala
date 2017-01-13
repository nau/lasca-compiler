package com.newniverse.parser

import com.newniverse.parser.LascaCompiler._

import scala.collection.mutable
import scala.scalanative.nir.Defn.Const
import scala.scalanative.nir._
import scala.scalanative.{nir, util}
import util._, util.ScopedVar.scoped


/**
  * Created by nau on 1/11/17.
  */
object NirGen {
  type Symbol = String
  private val curMethodEnv = new util.ScopedVar[MethodEnv]

  class MethodEnv(val fresh: Fresh) {
    private val env = mutable.Map.empty[Symbol, Val]

    def enter(sym: Symbol, value: Val): Unit =
      env += ((sym, value))

    def resolve(sym: Symbol): Val = env(sym)

  }

  def traverse[S](tree: Tree, state: S, f: (S, Tree) => S): S = tree match {
    case t@Package(_, stats) => stats.foldLeft(f(state, t)) { case (s, t) => traverse(t, s, f) }
    case t@Def(name, tpe, params, rhs) => traverse(rhs, f(state, t), f)
    case t@Block(stats, expr) =>
      val s = stats.foldLeft(state) { case (s, t) => traverse(t, s, f) }
      traverse(expr, s, f)
    case t@If(cond: Tree, thenp: Tree, elsep: Tree) =>
      traverse(elsep, traverse(thenp, traverse(cond, f(state, t), f), f), f)
    case t@Apply(fun, args: List[Tree]) =>
      val s = traverse(fun, f(state, t), f)
      args.foldLeft(s) { case (s, t) => traverse(t, s, f) }
    case t => f(state, t)
  }

  def traversePF[S](tree: Tree, state: S, f: PartialFunction[(S, Tree), S]): S = {
    def ff(state: S, tree: Tree) = if (f.isDefinedAt((state, tree))) f.apply((state, tree)) else state
    traverse(tree, state, ff)
  }

  private def stringConst(s: String) = {
    val name = s.take(5) + s.##.toString
    val array = Val.Array(Type.I8, s.getBytes("UTF-8").map(Val.I8) :+ Val.I8(0))
    name -> Defn.Const(Attrs.None, Global.Top(name), Type.Array(Type.I8, s.length + 1), array)
  }

  //  implicit val fresh = Fresh("tx")
  private implicit def fresh: Fresh = curMethodEnv.fresh

  def genNir(tree: Tree): Seq[Defn] = {
    //    val env = mutable.HashMap[String, Defn]

    scoped(curMethodEnv := new MethodEnv(Fresh("global"))) {
      val MainName = Global.Top("main")
      val MainSig = Type.Function(Seq(Arg(Type.I32), Arg(Type.Ptr)), Type.I32)
      val InitSig = Type.Function(Seq(), Type.Void)
      val Init = Val.Global(Global.Top("GC_init"), Type.Ptr)
      val InitDecl = Defn.Declare(Attrs.None, Init.name, InitSig)
      val argc = Val.Local(fresh(), Type.I32)
      val argv = Val.Local(fresh(), Type.Ptr)
      val putsTy = Type.Function(Seq(Arg(Type.Ptr)), Type.I32)
      val putsDeclare = Defn.Declare(Attrs.None, Global.Top("puts"), putsTy)
      val (id, helloDefn) = stringConst("Hello World")
      val main = Defn.Define(
        Attrs.None,
        MainName,
        MainSig,
        Seq(
          Inst.Label(fresh(), Seq(argc, argv)),
          Inst.Let(Op.Call(InitSig, Init, Seq())),
          Inst.Let(Op.Call(putsTy, Val.Global(Global.Top("puts"), putsTy), Seq(Val.Global(Global.Top(id), Type.Ptr)))),
          Inst.Ret(Val.I32(0))
        ))
      val lasca = genNir1(tree)
      Seq(InitDecl, putsDeclare, helloDefn, main) ++ lasca
    }
  }

  private def genNir1(tree: Tree): Seq[Defn] = {
    tree match {
      case Package(name, stats) => stats.flatMap(genNir1)
      case t@Def(name, tpe, params, rhs) =>
        val fresh = new Fresh("src")
        val env   = new MethodEnv(fresh)
        scoped(curMethodEnv := env) {
          val body = genExpr(rhs, Focus.start()(this.fresh))
          val inst = body.finish(Inst.Ret(body.value))
          Seq(Defn.Define(Attrs.None, Global.Top(name), Type.Function(Seq(), Type.I32), inst))
        }
      case t => println(t); Seq()
    }
  }

  private def genExpr(tree: Tree, focus: Focus): Focus = {
    tree match {
      case vd: ValDef =>
        val rhs = genExpr(vd.rhs, focus)
        curMethodEnv.enter(vd.name, rhs.value)
        rhs withValue Val.None
        //        case If(cond, thenp, elsep) =>
        //          val retty = genType(tree.tpe, box = false)
        //          genIf(retty, cond, thenp, elsep, focus)

        //        case app: Apply =>
        //          genApply(app, focus)

        //        case id: Ident =>
        /*val sym = id.symbol
        if (curMethodInfo.mutableVars.contains(sym)) {
          val ty = genType(sym.tpe, box = false)
          focus withOp Op.Load(ty, curMethodEnv.resolve(sym))
        } else if (sym.isModule) {
          focus withOp Op.Module(genTypeName(sym))
        } else {
          focus withValue (curMethodEnv.resolve(sym))
        }*/
        rhs

      case app: Apply => genApply(app, focus)

      case Ident(name) => focus withValue (curMethodEnv.resolve(name))

      case lit: Lit =>
        genLiteral(lit, focus)

      case block: Block => genBlock(block, focus)

      case EmptyTree =>
        focus

      case _ => focus

      //        case _ =>
      //          abort("Unexpected tree in genExpr: " + tree + "/" + tree.getClass)
    }
  }

  /* def genIf(retty: nir.Type,
    condp: Tree,
    thenp: Tree,
    elsep: Tree,
    focus: Focus) = {
    val thenn, elsen = fresh()

    val cond  = genExpr(condp, focus)
    val br    = cond.withIf(cond.value, Next(thenn), Next(elsen))

    merged(retty, br, Seq(
      focus => genExpr(thenp, focus.withLabel(thenn)),
      focus => genExpr(elsep, focus.withLabel(elsen))
    ))
  }*/

  def genBlock(block: Block, focus: Focus) = {
    val Block(stats, last) = block

    val focs = Focus.sequenced(stats, focus)(genExpr(_, _))
    val lastfocus = focs.lastOption.getOrElse(focus)
    genExpr(last, lastfocus)
  }

  def genApply(app: Apply, focus: Focus): Focus = {
    val Apply(fun, args) = app

    fun match {
      case Ident("+") if args.size == 1 => genSimpleOp(app, LascaPrimitives.POS, focus)
      case Ident("-") if args.size == 1 => genSimpleOp(app, LascaPrimitives.NEG, focus)
      case Ident("+") if args.size == 2 => genSimpleOp(app, LascaPrimitives.ADD, focus)
      case Ident("-") if args.size == 2 => genSimpleOp(app, LascaPrimitives.SUB, focus)
      case Ident("*") if args.size == 2 => genSimpleOp(app, LascaPrimitives.MUL, focus)
      case Ident("/") if args.size == 2 => genSimpleOp(app, LascaPrimitives.DIV, focus)
    }
  }

  def genSimpleOp(app: Apply, code: Int, focus: Focus): Focus = {
    val retty = Type.I32 //genType(app.tpe, box = false)
    val Apply(_, args: List[Tree]) = app

    args match {
      case List(right) => genUnaryOp(code, right, retty, focus)
      case List(left, right) => genBinaryOp(code, left, right, retty, focus)
      case _ => abort("Too many arguments for primitive function: " + app)
    }
  }

  def abort(s: String) = sys.error(s)

  def genUnaryOp(code: Int, rightp: Tree, opty: nir.Type, focus: Focus) = {
    import LascaPrimitives._
    val right = genExpr(rightp, focus)

    (opty, code) match {
      case (Type.I(_) | Type.F(_), POS) => right
      case (Type.F(_), NEG) => negateFloat(right.value, right)
      case (Type.I(_), NEG) => negateInt(right.value, right)
      case (Type.I(_), NOT) => negateBits(right.value, right)
      case (Type.I(_), ZNOT) => negateBool(right.value, right)
      case _ => abort("Unknown unary operation code: " + code)
    }
  }

  def numOfType(num: Int, ty: nir.Type) = ty match {
    case Type.I8 => Val.I8(num.toByte)
    case Type.I16 => Val.I16(num.toShort)
    case Type.I32 => Val.I32(num)
    case Type.I64 => Val.I64(num.toLong)
    case Type.F32 => Val.F32(num.toFloat)
    case Type.F64 => Val.F64(num.toDouble)
    case _ => unreachable
  }


  def negateInt(value: nir.Val, focus: Focus): Focus =
    focus withOp Op.Bin(Bin.Isub, value.ty, numOfType(0, value.ty), value)

  def negateFloat(value: nir.Val, focus: Focus): Focus =
    focus withOp Op.Bin(Bin.Fsub, value.ty, numOfType(0, value.ty), value)

  def negateBits(value: nir.Val, focus: Focus): Focus =
    focus withOp Op.Bin(Bin.Xor, value.ty, numOfType(-1, value.ty), value)

  def negateBool(value: nir.Val, focus: Focus): Focus =
    focus withOp Op.Bin(Bin.Xor, Type.Bool, Val.True, value)

  def genBinaryOp(code: Int,
    left: Tree,
    right: Tree,
    retty: nir.Type,
    focus: Focus): Focus = {
    import LascaPrimitives._

    val lty = Type.I32 //genType(left.tpe, box = false)
    val rty = Type.I32 //genType(right.tpe, box = false)
    val opty = Type.I32 //binaryOperationType(lty, rty)

    val binres = opty match {
      /*
            case Type.F(_) =>
              code match {
                case ADD =>
                  genBinaryOp(Op.Bin(Bin.Fadd, _, _, _), left, right, opty, focus)
                case SUB =>
                  genBinaryOp(Op.Bin(Bin.Fsub, _, _, _), left, right, opty, focus)
                case MUL =>
                  genBinaryOp(Op.Bin(Bin.Fmul, _, _, _), left, right, opty, focus)
                case DIV =>
                  genBinaryOp(Op.Bin(Bin.Fdiv, _, _, _), left, right, opty, focus)
                case MOD =>
                  genBinaryOp(Op.Bin(Bin.Frem, _, _, _), left, right, opty, focus)

                case EQ =>
                  genBinaryOp(Op.Comp(Comp.Feq, _, _, _), left, right, opty, focus)
                case NE =>
                  genBinaryOp(Op.Comp(Comp.Fne, _, _, _), left, right, opty, focus)
                case LT =>
                  genBinaryOp(Op.Comp(Comp.Flt, _, _, _), left, right, opty, focus)
                case LE =>
                  genBinaryOp(Op.Comp(Comp.Fle, _, _, _), left, right, opty, focus)
                case GT =>
                  genBinaryOp(Op.Comp(Comp.Fgt, _, _, _), left, right, opty, focus)
                case GE =>
                  genBinaryOp(Op.Comp(Comp.Fge, _, _, _), left, right, opty, focus)

                case _ =>
                  abort(
                    "Unknown floating point type binary operation code: " + code)
              }
      */

      //      case Type.I(_) =>
      case Type.I32 =>
        code match {
          case ADD =>
            genBinaryOp(Op.Bin(Bin.Iadd, _, _, _), left, right, opty, focus)
          case SUB =>
            genBinaryOp(Op.Bin(Bin.Isub, _, _, _), left, right, opty, focus)
          case MUL =>
            genBinaryOp(Op.Bin(Bin.Imul, _, _, _), left, right, opty, focus)
          case DIV =>
            genBinaryOp(Op.Bin(Bin.Sdiv, _, _, _), left, right, opty, focus)
          case MOD =>
            genBinaryOp(Op.Bin(Bin.Srem, _, _, _), left, right, opty, focus)

          case OR =>
            genBinaryOp(Op.Bin(Bin.Or, _, _, _), left, right, opty, focus)
          case XOR =>
            genBinaryOp(Op.Bin(Bin.Xor, _, _, _), left, right, opty, focus)
          case AND =>
            genBinaryOp(Op.Bin(Bin.And, _, _, _), left, right, opty, focus)
          case LSL =>
            genBinaryOp(Op.Bin(Bin.Shl, _, _, _), left, right, opty, focus)
          case LSR =>
            genBinaryOp(Op.Bin(Bin.Lshr, _, _, _), left, right, opty, focus)
          case ASR =>
            genBinaryOp(Op.Bin(Bin.Ashr, _, _, _), left, right, opty, focus)

          case EQ =>
            genBinaryOp(Op.Comp(Comp.Ieq, _, _, _), left, right, opty, focus)
          case NE =>
            genBinaryOp(Op.Comp(Comp.Ine, _, _, _), left, right, opty, focus)
          case LT =>
            genBinaryOp(Op.Comp(Comp.Slt, _, _, _), left, right, opty, focus)
          case LE =>
            genBinaryOp(Op.Comp(Comp.Sle, _, _, _), left, right, opty, focus)
          case GT =>
            genBinaryOp(Op.Comp(Comp.Sgt, _, _, _), left, right, opty, focus)
          case GE =>
            genBinaryOp(Op.Comp(Comp.Sge, _, _, _), left, right, opty, focus)

          //          case ZOR =>
          //            genIf(retty, left, Literal(Constant(true)), right, focus)
          //          case ZAND =>
          //            genIf(retty, left, right, Literal(Constant(false)), focus)

          case _ =>
            abort("Unknown integer type binary operation code: " + code)
        }

      /*
            case _: Type.RefKind =>
              code match {
                case EQ =>
                  genClassEquality(
                    left, right, ref = false, negated = false, focus)
                case NE =>
                  genClassEquality(left, right, ref = false, negated = true, focus)
                case ID =>
                  genClassEquality(left, right, ref = true, negated = false, focus)
                case NI =>
                  genClassEquality(left, right, ref = true, negated = true, focus)

                case _ => abort("Unknown reference type operation code: " + code)
              }
      */

      /*
            case Type.Ptr =>
              code match {
                case EQ | ID =>
                  genBinaryOp(Op.Comp(Comp.Ieq, _, _, _), left, right, opty, focus)
                case NE | NI =>
                  genBinaryOp(Op.Comp(Comp.Ine, _, _, _), left, right, opty, focus)
              }
      */

      case ty =>
        abort("Uknown binary operation type: " + ty)
    }

    binres // genCoercion(binres.value, binres.value.ty, retty, binres)
  }

  def genBinaryOp(op: (nir.Type, Val, Val) => Op,
    leftp: Tree,
    rightp: Tree,
    opty: nir.Type,
    focus: Focus): Focus = {
    val leftty = Type.I32 //genType(leftp.tpe, box = false)
    val left = genExpr(leftp, focus)
    val leftcoerced = left //genCoercion(left.value, leftty, opty, left)
    val rightty = Type.I32 //genType(rightp.tpe, box = false)
    val right = genExpr(rightp, leftcoerced)
    val rightcoerced = right //genCoercion(right.value, rightty, opty, right)

    rightcoerced withOp op(opty, leftcoerced.value, rightcoerced.value)
  }


  def genLiteral(lit: Lit, focus: Focus): Focus = {
    val v = lit.value match {
      case i: Int => Val.I32(i)
      case i: String => Val.String(i)
    }
    focus withValue v
  }
}
