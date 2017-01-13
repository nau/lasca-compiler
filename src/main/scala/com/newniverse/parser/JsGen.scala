package com.newniverse.parser

import javax.script.ScriptEngineManager

import com.newniverse.parser.LascaCompiler._

/**
  * Created by nau on 1/11/17.
  */
object JsGen {
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

  def runJs(js: String) = {
    val factory = new ScriptEngineManager(null)
    val engine = factory.getEngineByName("nashorn")
    engine.eval(JsGen.JsRuntime.globalFunctions + js)
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
      s"(function (){ $ss; })()"
    case If(cond, thenp, EmptyTree) => s"if (${toJs(cond)}) { ${toJs(thenp)} } "
    case If(cond, thenp, elsep) => s"(${toJs(cond)}) ? ( ${toJs(thenp)} ) : ( ${toJs(elsep)} )"
    case ValDef(name, _, body) => s"var $name = ${toJs(body)}"
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
