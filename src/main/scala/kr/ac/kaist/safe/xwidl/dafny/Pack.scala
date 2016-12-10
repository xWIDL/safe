package kr.ac.kaist.safe.xwidl.dafny

import kr.ac.kaist.safe.xwidl.spec._

object Pack {
  def packExpr(e: Expr): String = {
    e match {
      case BiOpExpr(le, op, re) => packExpr(le) + " " + op.toString + " " + packExpr(re)
      case IfThenElseExpr(cond, tb, eb) => "if(" + packExpr(cond) + ") then (" +
        packExpr(tb) + ") else (" + packExpr(eb) + ")"
      case VarExpr(_) => throw new Exception("VarExpr is not supported yet!")
      case LitExpr(lit) => lit match {
        case PrimBool(b) => b.toString
        case PrimInt(i) => i.toString
      }
    }

    //    def packInterface(i: Interface): String = {
    //      "interfacei.name
    //    }
  }
}
