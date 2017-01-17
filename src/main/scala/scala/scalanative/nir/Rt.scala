package scala.scalanative
package nir

import Type._

object Rt {
  val Object = Class(Global.Top("java.lang.Object"))
  val String = Class(Global.Top("java.lang.String"))
  val Type =
    Struct(Global.Top("lasca.runtime.Type"), Seq(I32, Ptr))
  val UnboxTy = Function(Seq(Arg(Ptr), Arg(I32)), Ptr)

  val BinOpTy = Function(Seq(Arg(I32), Arg(Ptr), Arg(Ptr)), Ptr)
  val BinOp = Defn.Declare(Attrs.None, Global.Top("runtimeBinOp"), BinOpTy)
}
