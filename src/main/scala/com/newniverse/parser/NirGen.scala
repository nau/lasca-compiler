package com.newniverse.parser

import com.newniverse.parser.LascaCompiler.{Type, _}

import scala.collection.mutable
import scala.scalanative.nir.{Type, _}
import scala.scalanative.util.ScopedVar.scoped
import scala.scalanative.util._
import scala.scalanative.{nir, util}
import Focus._
import scala.scalanative.nir.Type.Function


/**
  * Created by nau on 1/11/17.
  */

class DynamicNirGen extends NirGen {
  def genNir(ctx: Context): Seq[Defn] = {
    //    val env = mutable.HashMap[String, Defn]
    val tree = ctx.compilationUnit.untpdTree
    scoped(curMethodEnv := new MethodEnv(Fresh("main"))) {
      val rtType = Defn.Struct(Attrs.None, Rt.Type.name, Rt.Type.tys)
      val MainName = Global.Top("main")
      val MainSig = Type.Function(Seq(Arg(Type.I32), Arg(Type.Ptr)), Type.I32)
      val InitSig = Type.Function(Seq(), Type.Void)
      val Init = Val.Global(Global.Top("lasca_init"), Type.Ptr)
      val InitDecl = Defn.Declare(Attrs.None, Init.name, InitSig)
      val argc = Val.Local(fresh(), Type.I32)
      val argv = Val.Local(fresh(), Type.Ptr)
      val unbox = Defn.Declare(Attrs.None, Global.Top("unbox"), Rt.UnboxTy)
      val gcMallocTy = Type.Function(Seq(Arg(Type.I32)), Type.Ptr)
      val gcMalloc = Defn.Declare(Attrs.None, Global.Top("GC_malloc"), gcMallocTy)
      val putsTy = Type.Function(Seq(Arg(Type.Ptr)), Type.I32)
      val putsDeclare = Defn.Declare(Attrs.None, Global.Top("puts"), putsTy)
      val lascaMainSig = Type.Function(Seq(), Type.Ptr)

      val (id, helloDefn) = stringConst("Hello World")
      val main = Defn.Define(
        Attrs.None,
        MainName,
        MainSig,
        Seq(
          Inst.Label(fresh(), Seq(argc, argv)),
          Inst.Let(Op.Call(InitSig, Init, Seq())),
          Inst.Let(Op.Call(lascaMainSig, Val.Global(Global.Top("lascamain"), lascaMainSig), Seq())),
          Inst.Ret(Val.I32(0))
        ))
      val lasca = genNir1(ctx.compilationUnit.untpdTree)(ctx)
      Seq(Rt.BinOp, unbox, rtType, InitDecl, gcMalloc, putsDeclare, helloDefn, main) ++ lasca
    }
  }


  protected def genNir1(tree: Tree)(implicit ctx: Context): Seq[Defn] = {
    tree match {
      case Package(name, stats) => stats.flatMap(genNir1)
      case t@ExternDef(name, tpe, params) =>
        val ty = Type.Function(params.map(p => Arg(typeMapping(p.tpe))), typeMapping(tpe))
        Seq(Defn.Declare(Attrs.None, Global.Top(name), ty))
      case t@DefDef(name, tpe, params, rhs) =>
        val f = new Fresh("src")
        val env = new MethodEnv(f)
        scoped(curMethodEnv := env) {
          val params = genParams(t)
          val sig = Type.Function(params.map(p => Arg(p.valty)), typeMapping(tpe))
          val body = genExpr(rhs, Focus.start()(fresh).withLabel(fresh(), params: _*))
          val inst = body.finish(Inst.Ret(body.value))
          Seq(Defn.Define(Attrs.None, Global.Top(name), sig, inst))
        }
      case t@ValDef(name, _, Lit(v: Int, _)) => Seq(Defn.Const(Attrs.None, Global.Top(name), Type.I32, Val.I32(v)))
      case t@ValDef(name, _, Lit(v: String, _)) => Seq(Defn.Const(Attrs.None, Global.Top(name), Rt.String, Val.String(v)))
      case t => println(t); Seq()
    }
  }

  def genApply(app: Apply, focus: Focus)(implicit ctx: Context): Focus = {
    val Apply(fun, args) = app

    fun match {
      case _: Apply => genApply(flattenApply(app), focus)
      case Ident("+") if args.size == 1 => genSimpleOp(app, LascaPrimitives.POS, focus)
      case Ident("-") if args.size == 1 => genSimpleOp(app, LascaPrimitives.NEG, focus)
      case Ident("+") if args.size == 2 => genSimpleOp(app, LascaPrimitives.ADD, focus)
      case Ident("-") if args.size == 2 => genSimpleOp(app, LascaPrimitives.SUB, focus)
      case Ident("*") if args.size == 2 => genSimpleOp(app, LascaPrimitives.MUL, focus)
      case Ident("/") if args.size == 2 => genSimpleOp(app, LascaPrimitives.DIV, focus)
      case Ident("==") if args.size == 2 => genSimpleOp(app, LascaPrimitives.EQ, focus)
      case Ident("!=") if args.size == 2 => genSimpleOp(app, LascaPrimitives.NE, focus)
      case Ident(">") if args.size == 2 => genSimpleOp(app, LascaPrimitives.GT, focus)
      case Ident(">=") if args.size == 2 => genSimpleOp(app, LascaPrimitives.GE, focus)
      case Ident("<") if args.size == 2 => genSimpleOp(app, LascaPrimitives.LT, focus)
      case Ident("<=") if args.size == 2 => genSimpleOp(app, LascaPrimitives.LE, focus)
      case Ident(name) =>
        val funcType = genFuncType(name)
        val (as, last) = genSimpleArgs(args, focus)
        val (as1, last1) = genUnboxedArgs(as, last, funcType)
        val func = last1 withValue Val.Global(Global.Top(name), Type.Ptr)
        func withOp Op.Call(funcType, func.value, as1)
    }
  }


  def genUnboxedArgs(args: Seq[Val], last: Focus, funcType: Function): (Seq[Val], Focus) = {
    funcType.args.zip(args).foldLeft(Seq.empty[Val], last) { case ((args, f), (Arg(ty, _), v)) =>
      if (v.ty == Type.Ptr && ty == Type.I32) {
        val unboxed = unbox(f)
        (args :+ unboxed.value, unboxed)
      } else (args :+ v, f)
    }
  }

  def genLiteral(lit: Lit, focus: Focus): Focus = lit match {
    case Lit(v: Int, IntType) => box(Val.I32(lit.value.asInstanceOf[Int]), focus)
    case Lit(v: Boolean, BoolType) => box(Val.Bool(lit.value.asInstanceOf[Boolean]), focus)
    case _ => unsupported(s"literal $lit")
  }

  val types = Array(Type.Bool, Type.I32)
  val typeId: Map[nir.Type, Int] = types.zipWithIndex.toMap

  def box(v: Val, focus: Focus): Focus = {
    val f1 = genGcAlloc(4, focus)
    val f2 = genGcAlloc(8, f1)
    val tId = Val.I32(typeId(v.ty))
    val aggr = f2.withOp(Op.Store(v.ty, f1.value, v)).withOp(Op.Insert(Val.Undef(Rt.Type), tId, Seq(0)))
    val boxed = aggr.withOp(Op.Insert(aggr.value, f1.value, Seq(1)))
    boxed.withOp(Op.Store(Rt.Type, f2.value, boxed.value)).withValue(f2.value)
  }

  def unbox(f: Focus, tpe: nir.Type = Type.I32): Focus = {
    val rt = f.withOp(Op.Call(Rt.UnboxTy, Val.Global(Global.Top("unbox"), Rt.UnboxTy), Seq(f.value, Val.I32(typeId(tpe)))))
    rt.withOp(Op.Load(tpe, rt.value))
  }

  def genGcAlloc(size: Int, focus: Focus): Focus = {
    val ty = Type.Function(Seq(Arg(Type.I32)), Type.Ptr)
    focus.withOp(Op.Call(ty, Val.Global(Global.Top("GC_malloc"), ty), Seq(Val.I32(size))))
  }

  protected def typeMapping(lascaType: LascaCompiler.Type): nir.Type = lascaType match {
    case IntType => Type.I32
    case BoolType => Type.Bool
    case StringType => Type.Ptr
    case UnitType => Type.Void
    case AnyType => Type.Ptr
  }

  def genParams(dd: DefDef): Seq[Val.Local] = {
    val params = dd.params.map { case ValDef(sym, tpe, _) =>
      val name = fresh()
      val ty = typeMapping(tpe)
      val param = Val.Local(name, ty)
      curMethodEnv.enter(sym, param)
      param
    }

    params
  }


  protected def genExpr(tree: Tree, focus: Focus)(implicit ctx: Context): Focus = {
    tree match {
      case vd: ValDef =>
        val rhs = genExpr(vd.rhs, focus)
        curMethodEnv.enter(vd.name, rhs.value)
        rhs withValue Val.None
        rhs

      case If(cond, thenp, elsep) =>
        val retty = Type.Ptr //genType(tree.tpe, box = false)
        genIf(retty, cond, thenp, elsep, focus)


      case app: Apply => genApply(app, focus)

      case Ident(name) => focus withValue curMethodEnv.resolve(name)
      case lit: Lit => genLiteral(lit, focus)
      case block: Block => genBlock(block, focus)

      case EmptyTree =>
        focus

      case _ => focus

      //        case _ =>
      //          abort("Unexpected tree in genExpr: " + tree + "/" + tree.getClass)
    }
  }

  def genIf(retty: nir.Type,
    condp: Tree,
    thenp: Tree,
    elsep: Tree,
    focus: Focus)(implicit ctx: Context) = {
    val thenn, elsen = fresh()

    val cond = genExpr(condp, focus)
    val unboxed = unbox(cond, Type.Bool)
    val br = unboxed.withIf(unboxed.value, Next(thenn), Next(elsen))

    merged(retty, br, Seq(
      focus => genExpr(thenp, focus.withLabel(thenn)),
      focus => genExpr(elsep, focus.withLabel(elsen))
    ))
  }

  def genBlock(block: Block, focus: Focus)(implicit ctx: Context) = {
    val Block(stats, last) = block

    val focs = Focus.sequenced(stats, focus)(genExpr(_, _))
    val lastfocus = focs.lastOption.getOrElse(focus)
    genExpr(last, lastfocus)
  }


  def flattenApply(t: Apply): Apply = {
    t match {
      case Apply(ap: Apply, List(arg)) =>
        val inner = flattenApply(ap)
        inner.copy(args = inner.args :+ arg)
      case ap@Apply(Ident(name), List(arg)) => ap
    }
  }

  def genFuncType(name: String)(implicit ctx: Context): Type.Function = {
    val defDef = ctx.scope(Symbol(name))
    val (params, dtpe) = defDef match {
      case d: ExternDef => (d.params.map(p => Arg(typeMapping(p.tpe))), typeMapping(d.tpe))
      case d: DefDef => (d.params.map(_ => Arg(Type.Ptr)), Type.Ptr)
    }
    Type.Function(params, dtpe)
  }

  def genSimpleArgs(argsp: Seq[Tree], focus: Focus)(implicit ctx: Context): (Seq[Val], Focus) = {
    val args = Focus.sequenced(argsp, focus)(genExpr(_, _))
    val argvalues = args.map(_.value)
    val last = args.lastOption.getOrElse(focus)

    (argvalues, last)
  }

  def genSimpleOp(app: Apply, code: Int, focus: Focus)(implicit ctx: Context): Focus = {
    val retty = Type.I32 //genType(app.tpe, box = false)
    val Apply(_, args: List[Tree]) = app

    args match {
      case List(right) => abort("BBBB")
      case List(left, right) => genBinaryOp(code, left, right, retty, focus)
      case _ => abort("Too many arguments for primitive function: " + app)
    }
  }

  def abort(s: String) = sys.error(s)

  def genBinaryOp(code: Int,
    left: Tree,
    right: Tree,
    retty: nir.Type,
    focus: Focus)(implicit ctx: Context): Focus = {
    val lexp = genExpr(left, focus)
    val rexp = genExpr(right, lexp)

    rexp.withOp(Op.Call(Rt.BinOpTy, Val.Global(Rt.BinOp.name, Rt.BinOpTy), Seq(Val.I32(code), lexp.value, rexp.value)))
  }

}

trait NirGen {
  type Symbol = String
  protected val curMethodEnv = new util.ScopedVar[MethodEnv]

  class MethodEnv(val fresh: Fresh) {
    private val env = mutable.Map.empty[Symbol, Val]

    def enter(sym: Symbol, value: Val): Unit =
      env += ((sym, value))

    def resolve(sym: Symbol): Val = env(sym)

    def resolveOpt(sym: Symbol): Option[Val] = env.get(sym)
  }

  protected def stringConst(s: String) = {
    val name = s.take(5) + s.##.toString
    val array = Val.Array(Type.I8, s.getBytes("UTF-8").map(Val.I8) :+ Val.I8(0))
    name -> Defn.Const(Attrs.None, Global.Top(name), Type.Array(Type.I8, s.length + 1), array)
  }

  //  implicit val fresh = Fresh("tx")
  protected implicit def fresh: Fresh = curMethodEnv.fresh


}
