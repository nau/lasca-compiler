package com.newniverse.parser

import javax.script.ScriptEngineManager

import com.newniverse.parser.LascaParser._
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

import scala.collection.JavaConverters._

/**
  * Created by Alexander Nemish on 12/26/16.
  */
object LascaCompiler {


  sealed trait Tree
  case object EmptyTree extends Tree
  case class Ident(name: String) extends Tree
  case class Val(name: String, tpe: Type = AnyType, rhs: Tree = EmptyTree) extends Tree
  case class Params(params: List[Val]) extends Tree
  case class Def(name: String, tpe: Type = AnyType, params: List[Val], rhs: Tree) extends Tree
  case class Lit(value: Any, tpe: Type) extends Tree
  case class Block(stats: List[Tree], expr: Tree) extends Tree
  case class Apply(fun: Tree, args: List[Tree]) extends Tree
  case class If(cond: Tree, thenp: Tree, elsep: Tree) extends Tree
  case class Package(name: String, stats: List[Tree]) extends Tree

  sealed trait Type extends Tree
  case object AnyType extends Type
  case object IntType extends Type
  case object StringType extends Type
  case object BoolType extends Type


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


    override def visitValDef(ctx: ValDefContext): Tree = {
      Val(ctx.Id().getText, AnyType, visit(ctx.expr()))
    }

    override def visitDefDef(ctx: DefDefContext): Tree = {
      val name = ctx.Id().getText
      val params = Option(ctx.paramClause()).toList.flatMap(p => visit(p).asInstanceOf[Params].params)
      val body = visit(ctx.expr())
      Def(name, AnyType, params, body)
    }

    override def visitParamClause(ctx: ParamClauseContext): Tree = {
      visit(ctx.params())
    }

    override def visitParams(ctx: ParamsContext): Tree = {
      val params = ctx.param().asScala.map(p => visit(p).asInstanceOf[Val]).toList
      Params(params)
    }

    override def visitParam(ctx: ParamContext): Tree = {
      val id = ctx.Id().getText
      val tpe = Option(ctx.`type`()).map(_.getText).getOrElse(AnyType)
      Val(id, AnyType, EmptyTree)
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
      Package("main", (vals ++ defs).toList)
    }
  }

  def readFile(n: String) = {
    val source = scala.io.Source.fromFile(n)
    val lines = try source.mkString finally source.close()
    lines
  }

  def main(args: Array[String]): Unit = {
    val code = readFile("example1.nl")
    val tree = parse(code)
    val js = toJs(tree)
    println(js)
    runJs(JsRuntime.globalFunctions + js)
  }

  def runJs(js: String) = {
    val factory = new ScriptEngineManager(null)
    val engine = factory.getEngineByName("nashorn")
    engine.eval(js)
  }

  def parse(code: String): Tree = {
    val input = new ANTLRInputStream(code)

    // create a lexer that feeds off of input CharStream
    val lexer = new LascaLexer(input)

    // create a buffer of tokens pulled from the lexer
    val tokens = new CommonTokenStream(lexer)

    // create a parser that feeds off the tokens buffer
    val parser = new LascaParser(tokens)
    parser.setBuildParseTree(true)

    val visitor = new Visitor
    val tree = parser.compilationUnit() // begin parsing at init rule
    val ast = tree.accept(visitor)
    println(tree.toStringTree(parser))
    println(code)
    println(ast)
    ast
  }

  val jsoperators = Map("or" -> "||", "and" -> "&&", "xor" -> "^") ++ List("==", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/", "<<", ">>").map(o => (o, o))
  val jsUnaryOps = Set("-" , "+" , "~" , "!")
  object JsRuntime {
    val globalFunctions =
      """
        |function callOrIdent(param) { return (typeof param === 'function') ? param() : param; }
      """.stripMargin
  }

  private def flattenApply(t: Apply): Apply = {
    t match {
      case Apply(ap: Apply, List(arg)) =>
        val inner = flattenApply(ap)
        inner.copy(args = inner.args :+ arg)
      case ap@Apply(Ident(name), List(arg)) => ap
    }
  }

  def toJs(tree: Tree): String = tree match {
    case Package(name, stats) =>
      val ss = stats.map(toJs).mkString(";\n")
      s"(function package_$name() {$ss})();\n"
    case Def(name, _, params, body) =>
      val ps = params.map(_.name).mkString(",")

      val b = body match {
        case Block(stats, expr) =>
          val ss = stats.map(toJs).mkString(";\n")
          s"{ $ss;\nreturn ${toJs(expr)}; }"
        case e => s"{ return ${toJs(e)}; }"
      }
      s"function $name($ps) $b"
    case Block(stats, expr) =>
      val ss = (stats :+ expr).map(toJs).mkString(";\n")
      s"{ $ss; }"
    case If(cond, thenp, EmptyTree) => s"if (${toJs(cond)}) { ${toJs(thenp)} } "
    case If(cond, thenp, elsep) => s"(${toJs(cond)}) ? ( ${toJs(thenp)} ) : ( ${toJs(elsep)} )"
    case Val(name, _, body) => s"var $name = ${toJs(body)}"
    case ap@Apply(_:Apply, List(arg)) => toJs(flattenApply(ap))
    case Apply(Ident(op), List(rhs)) if jsUnaryOps contains op => s"$op(${toJs(rhs)})"
    case Apply(Ident(op), List(lhs, rhs)) if jsoperators contains op => s"(${toJs(lhs)} ${jsoperators(op)} ${toJs(rhs)})"
    case Apply(Ident(fun), args) =>
      val as = args.map(toJs).mkString(", ")
      s"$fun($as)"
    case Apply(fun, args) =>
      val as = args.map(toJs).mkString(", ")
      s"${toJs(fun)}($as)"
    case Ident(name) => s"callOrIdent($name)"
    case Lit(v: Int, IntType) => v.toString
    case Lit(v: Boolean, BoolType) => v.toString
    case Lit(v: String, StringType) => v.toString
    case EmptyTree => ""
  }
}
