package com.newniverse.parser

import com.newniverse.parser.LascaCompiler._
import com.newniverse.parser.LascaParser._
import scala.collection.JavaConverters._

/**
  * Created by nau on 1/16/17.
  */
class Visitor extends LascaBaseVisitor[Tree] {
  override def visitIdent(ctx: IdentContext): Tree = Ident(ctx.Id.getText)

  override def visitStringLit(ctx: StringLitContext): Tree = {
    Lit(ctx.getText, StringType)
  }

  override def visitInteger(ctx: IntegerContext): Tree = {
    Lit(ctx.getText.replace("_", "").toInt, IntType)
  }

  override def visitBoolean(ctx: BooleanContext): Tree = {
    Lit(ctx.getText.toBoolean, BoolType)
  }

  private def typeMapping(s: String) = {
    println(s)
    s match {
      case "Int" => IntType
      case "Bool" => BoolType
      case "String" => StringType
      case "Unit" => UnitType
      case _ => AnyType
    }
  }
  override def visitValDef(ctx: ValDefContext): Tree = {
    val explicitTpe = Option(ctx.`type`()).map(t => typeMapping(t.TypeId().getText))
    val rhs = visit(ctx.expr())
    val infered = (explicitTpe, rhs) match {
      case (Some(tpe), _) => tpe
      case (None, ValDef(_, tpe, _)) => tpe
      case (None, Lit(_, tpe)) => tpe
      case _ => AnyType
    }
    ValDef(ctx.Id().getText, infered, rhs)
  }

  override def visitExternDef(ctx: ExternDefContext): Tree = {
    val name = ctx.Id().getText
    val params = Option(ctx.paramClause()).toList.flatMap(p => visitParamClause(p).asInstanceOf[Params].params)
    val retty = Option(ctx.`type`()).map { t =>
      println(t.TypeId().getText)
      typeMapping(t.TypeId().getText)
    } getOrElse AnyType
    ExternDef(name, retty, params)
  }

  override def visitDefDef(ctx: DefDefContext): Tree = {
    val name = ctx.Id().getText
    val params = Option(ctx.paramClause()).toList.flatMap(p => visit(p).asInstanceOf[Params].params)
    val body = visit(ctx.expr())
    DefDef(name, AnyType, params, body)
  }

  override def visitParamClause(ctx: ParamClauseContext): Tree = {
    visit(ctx.params())
  }

  override def visitParams(ctx: ParamsContext): Tree = {
    val params = ctx.param().asScala.map(p => visit(p).asInstanceOf[ValDef]).toList
    Params(params)
  }

  override def visitParam(ctx: ParamContext): Tree = {
    val id = ctx.Id().getText
    val tpe = Option(ctx.`type`()).map(t => typeMapping(t.getText)).getOrElse(AnyType)
    ValDef(id, tpe, EmptyTree)
  }


  override def visitIfExpr(ctx: IfExprContext): Tree = {
    val exprs = ctx.expr()
    val cond = visit(exprs.get(0))
    val thenp = visit(exprs.get(1))
    val elsep = if (exprs.size > 2) visit(exprs.get(2)) else EmptyTree
    If(cond, thenp, elsep)
  }

  override def visitExpr(ctx: ExprContext): Tree = super.visitExpr(ctx)

  override def visitInfixExpr(ctx: InfixExprContext): Tree = {
    if (ctx.prefixExpr() != null) visit(ctx.prefixExpr())
    else {

      ctx.infixExpr().asScala.foreach(e => println(e.getText))

      val lhs = visit(ctx.infixExpr(0))
      val op = Ident(ctx.op.getText)
      val rhs = visit(ctx.infixExpr(1))
      Apply(op, List(lhs, rhs))
    }
  }

  override def visitPrefixExpr(ctx: PrefixExprContext): Tree = {
    val expr = if (ctx.blockExpr != null) visit(ctx.blockExpr()) else visit(ctx.simpleExpr1())
    if (ctx.UnaryOp() != null)
      Apply(Ident(ctx.UnaryOp().getText), List(expr))
    else expr
  }


  override def visitSimpleExpr1(ctx: SimpleExpr1Context): Tree = {
    if (ctx.simpleExpr1() != null) {
      val fun = visit(ctx.simpleExpr1())
      val argExp = ctx.argumentExprs()
      val args = if (argExp.exprs() != null) ctx.argumentExprs().exprs().expr().asScala.map(visit).toList
      else List(visit(argExp.simpleExpr2()))
      Apply(fun, args)
    } else visit(ctx.simpleExpr2())
  }

  override def visitSimpleExpr2(ctx: SimpleExpr2Context): Tree = {
    if (ctx.ex != null) visit(ctx.ex)
    else super.visitSimpleExpr2(ctx)
  }

  override def visitBlockExpr(ctx: BlockExprContext): Tree = visit(ctx.block())

  override def visitBlock(ctx: BlockContext): Tree = {
    val statLen = ctx.blockStat().size()
    val stats = ctx.blockStat().asScala.map(visit).toList
    val expr = ctx.expr()
    if (expr == null && statLen == 0) {
      Block(Nil, EmptyTree)
    } else if (expr == null && statLen > 0) {
      val (init, last) = stats.splitAt(statLen - 1)
      Block(init, last.head)
    } else {
      val e = visit(expr)
      Block(stats, e)
    }
  }

  override def visitBlockStat(ctx: BlockStatContext): Tree = {
    val valdef = Option(ctx.valDef()).map(visit)
    val defdef = Option(ctx.defDef()).map(visit)
    def expr = Option(ctx.expr()).map(visit)
    valdef orElse defdef orElse expr getOrElse EmptyTree
  }

  override def visitType(ctx: TypeContext): Tree = AnyType

  override def visitArgumentExprs(ctx: ArgumentExprsContext): Tree = super.visitArgumentExprs(ctx)

  override def visitExprs(ctx: ExprsContext): Tree = super.visitExprs(ctx)

  override def visitCompilationUnit(ctx: CompilationUnitContext): Tree = {
    val vals = ctx.valDef().asScala.map(dd => this.visit(dd))
    val defs = ctx.defDef().asScala.map(dd => this.visit(dd))
    val externs = ctx.externDef().asScala.map(dd => this.visit(dd))
    Package("main", (externs ++ vals ++ defs).toList)
  }
}

