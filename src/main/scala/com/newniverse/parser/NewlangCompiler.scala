package com.newniverse.parser

import com.newniverse.parser.NewlangParser.CompilationUnitContext
import org.antlr.v4.runtime.{ANTLRInputStream, CommonTokenStream}

/**
  * Created by Alexander Nemish on 12/26/16.
  */
object NewlangCompiler {
  def main(args: Array[String]): Unit = {
    val code =
      """def test""".stripMargin

    val tree = parse(code)
    println(toJs(tree)) // print LISP-style tree
  }

  def parse(code: String): CompilationUnitContext = {
    val input = new ANTLRInputStream(code)

    // create a lexer that feeds off of input CharStream
    val lexer = new NewlangLexer(input)

    // create a buffer of tokens pulled from the lexer
    val tokens = new CommonTokenStream(lexer)

    // create a parser that feeds off the tokens buffer
    val parser = new NewlangParser(tokens)

    val tree = parser.compilationUnit() // begin parsing at init rule
    println(tree.toStringTree(parser))
    tree
  }

  def toJs(tree: CompilationUnitContext): String = {
//    println(tree.topStatSeq().topStat().get(0).`def`().Id().getSymbol.getText)
    ""
  }


}
