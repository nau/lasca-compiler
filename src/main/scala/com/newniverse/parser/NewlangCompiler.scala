package com.newniverse.parser

import javax.script.ScriptEngineManager

import com.newniverse.parser.NewlangParser._
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

import scala.collection.JavaConverters._

/**
  * Created by Alexander Nemish on 12/26/16.
  */
object NewlangCompiler {


  sealed trait Tree
  case object EmptyTree extends Tree
  case class Ident(name: String) extends Tree
  case class Val(name: String, tpe: Type = AnyType, rhs: Tree = EmptyTree) extends Tree
  case class Params(params: List[Val]) extends Tree
  case class Def(name: String, tpe: Type = AnyType, params: List[Val], rhs: Tree) extends Tree
  case class Lit(value: Any, tpe: Type) extends Tree
  case class Block(stats: List[Tree], expr: Tree) extends Tree
  case class Apply(fun: Tree, args: List[Tree]) extends Tree
  case class Package(name: String, stats: List[Tree]) extends Tree

  sealed trait Type extends Tree
  case object AnyType extends Type
  case object IntType extends Type
  case object StringType extends Type
  case object BoolType extends Type


  class Visitor extends NewlangBaseVisitor[Tree] {
    override def visitIdent(ctx: IdentContext): Tree = Ident(ctx.Id.getText)

    override def visitStringLit(ctx: StringLitContext): Tree = {
      println(s"Found Lit ${ctx.getText}")
      Lit(ctx.getText, StringType)
    }

    override def visitInteger(ctx: IntegerContext): Tree = {
      println(s"Found Lit ${ctx.getText}")
      Lit(ctx.getText.toInt, IntType)
    }

    override def visitBoolean(ctx: BooleanContext): Tree = {
      println(s"Found Lit ${ctx.getText}")
      Lit(ctx.getText.toBoolean, BoolType)
    }

    override def visitDefDef(ctx: DefDefContext): Tree = {
      println("Here")
      val name = ctx.Id().getText
      val params = Option(ctx.paramClause()).toList.flatMap(p => visit(p).asInstanceOf[Params].params)
      val body = visit(ctx.expr())
      println(s"Found Def ${name}")
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
      println(s"Found param $id: $tpe")
      Val(id, AnyType, EmptyTree)
    }

    override def visitExpr(ctx: ExprContext): Tree = {
      if (ctx.argumentExprs() != null) {
        val args = ctx.argumentExprs().exprs().expr().asScala.map(visit).toList
        Apply(visit(ctx.expr()), args)
      } else visitChildren(ctx)
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
      val defdef = Option(ctx.defDef()).map(visit)
      def expr = Option(ctx.expr()).map(visit)
      defdef orElse expr getOrElse EmptyTree
    }

    override def visitType(ctx: TypeContext): Tree = AnyType

    override def visitArgumentExprs(ctx: ArgumentExprsContext): Tree = super.visitArgumentExprs(ctx)

    override def visitExprs(ctx: ExprsContext): Tree = super.visitExprs(ctx)

    override def visitCompilationUnit(ctx: CompilationUnitContext): Tree = {
      println("AAA")
      val defs = ctx.defDef().asScala.map(dd => this.visit(dd))
      Package("main", defs.toList)
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
    println(js) // print LISP-style tree
    runJs(js)
  }

  def runJs(js: String) = {
    val factory = new ScriptEngineManager(null)
    val engine = factory.getEngineByName("nashorn")
    engine.eval(js)
  }

  def parse(code: String): Tree = {
    val input = new ANTLRInputStream(code)

    // create a lexer that feeds off of input CharStream
    val lexer = new NewlangLexer(input)

    // create a buffer of tokens pulled from the lexer
    val tokens = new CommonTokenStream(lexer)

    // create a parser that feeds off the tokens buffer
    val parser = new NewlangParser(tokens)
    parser.setBuildParseTree(true)

    val visitor = new Visitor
    val tree = parser.compilationUnit() // begin parsing at init rule
    val ast = tree.accept(visitor)
    println(tree.toStringTree(parser))
    println(ast)
    ast
  }

  def toJs(tree: Tree): String = tree match {
    case Package(name, stats) =>
      val ss = stats.map(toJs).mkString(";\n")
      s"(function package_$name() {$ss})();\n"
    case Def(name, _, params, body) =>
      val ps = params.map(_.name).mkString(",")

      val b = body match {
        case e: Lit => s"{ return ${toJs(e)}; }"
        case Block(stats, expr) =>
          val ss = stats.map(toJs).mkString(";\n")
          s"{ $ss;\nreturn ${toJs(expr)}; }"
      }
      s"function $name($ps) $b"
    case Apply(fun, args) =>
      val as = args.map(toJs).mkString(", ")
      s"(${toJs(fun)})($as)"
    case Ident(name) => name
    case Lit(v: Int, IntType) => v.toString
    case Lit(v: Boolean, BoolType) => v.toString
    case Lit(v: String, StringType) => v.toString
    case EmptyTree => ""
  }
}
