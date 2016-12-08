package kr.ac.kaist.safe.xwidl.dafny

import kr.ac.kaist.safe.analyzer.domain.{AbsState, AbsValue}
import kr.ac.kaist.safe.xwidl.spec._

object ExprCodeGen {
  def emit(op: Operation,
           absArgs: List[AbsValue],
           st: AbsState): Unit = {
    // Only ensures are processed now


  }

  def gamma(absValue: AbsValue) : Predicate = {
    if (absValue.locset.isBottom) {
      val pVal = absValue.pvalue

    } else {
      Predicate("x", TyNum, LitExpr(PrimBool(true))) // FIXME
    }
  }

}
