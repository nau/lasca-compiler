package com.newniverse.parser

import com.newniverse.parser.LascaCompiler.{CompilationUnit, Tree}

import scala.collection.mutable
import scala.collection.mutable.HashMap

/**
  * Created by nau on 1/16/17.
  */

case class Symbol(name: String)


class Context(cu: CompilationUnit) extends Cloneable {
  var outer: Context = NoContext
  var owner: Symbol = null
  val compilationUnit: CompilationUnit = cu
  val scope: mutable.Map[Symbol, Tree] = mutable.HashMap()
  def fresh: Context = {
    val ctx = clone.asInstanceOf[Context]
    ctx.outer = this
    ctx
  }
  def enter(sym: Symbol, tree: Tree): Symbol = {
    this.scope.put(sym, tree)
    sym
  }
}


object NoContext extends Context(null) {
//  val base = null

}