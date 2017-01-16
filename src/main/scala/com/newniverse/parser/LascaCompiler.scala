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
  trait Def extends Tree {
    def name: String
  }
  case class ValDef(name: String, tpe: Type = AnyType, rhs: Tree = EmptyTree) extends Tree with Def
  case class Params(params: List[ValDef]) extends Tree
  case class ExternDef(name: String, tpe: Type = AnyType, params: List[ValDef]) extends Tree with Def
  case class DefDef(name: String, tpe: Type = AnyType, params: List[ValDef], rhs: Tree) extends Tree with Def
  case class Lit(value: Any, tpe: Type) extends Tree
  case class Block(stats: List[Tree], expr: Tree) extends Tree
  case class Apply(fun: Tree, args: List[Tree]) extends Tree
  case class If(cond: Tree, thenp: Tree, elsep: Tree) extends Tree
  case class Package(name: String, stats: List[Tree]) extends Tree

  case class SourceFile(file: File, content: Array[Char])
  case class CompilationUnit(source: SourceFile) {
    var untpdTree: Tree = EmptyTree
    var tpdTree: Tree = EmptyTree
  }

  sealed trait Type extends Tree
  case object AnyType extends Type
  case object IntType extends Type
  case object StringType extends Type
  case object BoolType extends Type
  case object UnitType extends Type



  def readFile(n: String) = {
    val source = scala.io.Source.fromFile(n)
    val lines = try source.mkString finally source.close()
    lines
  }

  def toLlvm(ctx: Context) = {

    val module = NirGen.genNir(ctx)
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

  def createSymbol(mdef: Def) = Symbol(mdef.name)

  def namer(ctx: Context) = {
    val unit = ctx.compilationUnit
    val tree = unit.untpdTree
    def recur(stat: Tree): Context = stat match {
      case pcl: Package =>
        pcl.stats.foreach(recur)
        ctx
      case mdef: Def =>
        val sym = createSymbol(mdef)
        ctx.enter(sym, mdef)
        ctx
      case _ => ctx
    }
    recur(tree)
  }

  def main(args: Array[String]): Unit = {
    val code = readFile("hello.lasca")
    val source = SourceFile(new File("hello.lasca"), code.toCharArray)
    var compUnit = CompilationUnit(source)
    compUnit = parse(compUnit)
    val ctx = new Context(compUnit)
    namer(ctx)
    val js = JsGen.toJs(compUnit.untpdTree)
    println(js)
    JsGen.runJs(js)
    val llvm = toLlvm(ctx)
    println(llvm)
    val path = compile(llvm)
    println(exec(path))
  }

  def parse(compilationUnit: CompilationUnit): CompilationUnit = {
    val source = compilationUnit.source
    val input = new ANTLRInputStream(source.content, source.content.length)

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
    println(source)
    println(ast)
    compilationUnit.untpdTree = ast
    compilationUnit
  }


}
