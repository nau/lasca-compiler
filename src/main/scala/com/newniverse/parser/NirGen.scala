package com.newniverse.parser

import com.newniverse.parser.LascaCompiler._

import scala.collection.mutable
import scala.scalanative.nir.Defn.Const
import scala.scalanative.nir._
import scala.scalanative.{nir, util}

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

  implicit val fresh = Fresh("tx")
  def genNir(tree: Tree): Seq[Defn] = {
//    val env = mutable.HashMap[String, Defn]


    val MainName = Global.Top("main")
    val MainSig = Type.Function(Seq(Arg(Type.I32), Arg(Type.Ptr)), Type.I32)
    val InitSig  = Type.Function(Seq(), Type.Void)
    val Init     = Val.Global(Global.Top("GC_init"), Type.Ptr)
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

  private def genNir1(tree: Tree): Seq[Defn] = {
    tree match {
      case Package(name, stats) => stats.flatMap(genNir1)
      case t@Def(name, tpe, params, rhs) =>
        val body = genExpr(rhs, Focus.start())
        val inst = body.finish(Inst.Ret(body.value))
        Seq(Defn.Define(Attrs.None, Global.Top(name), Type.Function(Seq(), Type.I32), inst))
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
        focus

      case lit: Lit =>
        genLiteral(lit, focus)

      //        case block: Block =>
      //genBlock(block, focus)

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
  }

  def genBlock(block: Block, focus: Focus) = {
    val Block(stats, last) = block

    def isCaseLabelDef(tree: Tree) =
      tree.isInstanceOf[LabelDef] && hasSynthCaseSymbol(tree)

    def translateMatch(last: LabelDef) = {
      val (prologue, cases) = stats.span(s => !isCaseLabelDef(s))
      val labels            = cases.map { case label: LabelDef => label }
      genMatch(prologue, labels :+ last, focus)
    }

    last match {
      case label: LabelDef if isCaseLabelDef(label) =>
        translateMatch(label)

      case Apply(
      TypeApply(Select(label: LabelDef, nme.asInstanceOf_Ob), _), _)
        if isCaseLabelDef(label) =>
        translateMatch(label)

      case _ =>
        val focs      = sequenced(stats, focus)(genExpr(_, _))
        val lastfocus = focs.lastOption.getOrElse(focus)
        genExpr(last, lastfocus)
    }
  }
  def genApply(app: Apply, focus: Focus): Focus = {
    val Apply(fun, args) = app

    fun match {
      case _: TypeApply =>
        genApplyTypeApply(app, focus)
      case Select(Super(_, _), _) =>
        genMethodCall(fun.symbol,
          statically = true,
          curMethodThis.get.get,
          args,
          focus)
      case Select(New(_), nme.CONSTRUCTOR) =>
        genApplyNew(app, focus)
      case _ =>
        val sym = fun.symbol

        if (sym.isLabel) {
          genApplyLabel(app, focus)
        } else if (scalaPrimitives.isPrimitive(sym)) {
          genPrimitiveOp(app, focus)
        } else if (currentRun.runDefinitions.isBox(sym)) {
          val arg = args.head
          genPrimitiveBox(arg.tpe, arg, focus)
        } else if (currentRun.runDefinitions.isUnbox(sym)) {
          genPrimitiveUnbox(app.tpe, args.head, focus)
        } else {
          val Select(receiverp, _) = fun
          genMethodCall(
            fun.symbol, statically = false, receiverp, args, focus)
        }
    }
  }
*/
  def genLiteral(lit: Lit, focus: Focus): Focus = {
    val v = lit.value match {
      case i: Int => Val.I32(i)
      case i: String => Val.String(i)
    }
    focus withValue v
  }
}
