package com.newniverse.parser

import java.io.{File, PrintWriter}

import com.newniverse.parser.LascaParser._
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

import scala.collection.JavaConverters._
import scala.util.Try

/**
  * Created by Alexander Nemish on 12/26/16.
  */
object LascaCompiler {


  sealed trait Tree
  case object EmptyTree extends Tree
  case class Ident(name: String) extends Tree
  case class ValDef(name: String, tpe: Type = AnyType, rhs: Tree = EmptyTree) extends Tree
  case class Params(params: List[ValDef]) extends Tree
  case class ExternDef(name: String, tpe: Type = AnyType, params: List[ValDef]) extends Tree
  case class Def(name: String, tpe: Type = AnyType, params: List[ValDef], rhs: Tree) extends Tree
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
  case object UnitType extends Type


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
      Def(name, AnyType, params, body)
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

  def readFile(n: String) = {
    val source = scala.io.Source.fromFile(n)
    val lines = try source.mkString finally source.close()
    lines
  }

  def toLlvm(tree: Tree) = {

    val module = NirGen.genNir(tree)
    println(module)
    CodeGen.apply(module).genIrString
  }

  def compile(llvm: String) = {
    import scala.sys.process._
    val clangpp   = "clang++"
    val clangOpts = Seq.empty[String]
    val clang     = "clang"
    val fname = "filename.ll"
    val includes = {
      val includedir =
        Try(Process("llvm-config --includedir").lineStream_!.toSeq)
          .getOrElse(Seq.empty)
      ("/usr/local/include" +: includedir).map(s => s"-I$s")
    }
    val libs = {
      val libdir =
        Try(Process("llvm-config --libdir").lineStream_!.toSeq).getOrElse(Seq.empty)
      ("/usr/local/lib" +: libdir).map(s => s"-L$s")
    }

    def abs(file: File): String = file.getAbsolutePath

    /** Compiles *.c[pp] in `srcPath`. */
    def compileCSources(clang: String, srcPath: File, outPath: File): Boolean = {
      val cpaths = srcPath.listFiles().collect { case f if f.getName.endsWith(".c") => abs(f) }
      val compilec   = clang +: (includes ++ ("-c" +: cpaths)) toList
//      val compilecpp = abs(clangpp) +: (includes ++ ("-c" +: cpppaths))

      println(s"Compiling Runtime ${compilec.mkString(" ")}")
      val cExit = Process(compilec, outPath).lineStream.toList.map(println)

//      val cppExit = Process(compilecpp, srcPath) ! logger
      true
    }

    val runtimeOPath = new File("target/runtime")
    runtimeOPath.mkdir()
    compileCSources(clang, new File("src/main/c"), runtimeOPath)

    val runtimeOFiles = runtimeOPath.listFiles().collect { case f if f.getName.endsWith(".o") => abs(f) }

    new PrintWriter(fname) { write(llvm); close }
    (s"opt -O2 $fname" #| "llvm-dis").lineStream.toList.map(println)
    val opts = (includes ++ libs ++ Seq("-lgc", "-o", "test.out", "filename.ll") ++ runtimeOFiles).toList
    println(opts)
    Process(clang, opts).!
    new File("test.out").getAbsolutePath
  }

  def exec(path: String): Unit = {
    import scala.sys.process._
    path.!
  }

  def main(args: Array[String]): Unit = {
    val code = readFile("hello.ls")
    val tree = parse(code)
    val js = JsGen.toJs(tree)
    println(js)
    JsGen.runJs(js)
    val llvm = toLlvm(tree)
    println(llvm)
    val path = compile(llvm)
    println(exec(path))
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


}
