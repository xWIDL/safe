package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.xwidl.solver.PackZ3
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Type extends PackZ3 {
  def absTopPVal: AbsPValue

  def absTopVal: AbsValue = AbsValue(absTopPVal)

  def concValList: List[ConcVal]
}

sealed trait PrimType extends Type

case object TyNum extends PrimType {
  def absTopPVal: AbsPValue = DefaultNumber.Top

  def concValList: List[ConcVal] =
    // it is really hard to imagine how to represent this in a constraint solver
    // Maybe full abstraction is *ultimately* the right way to go
    List(
      PredicateVal("x", TyNum, LitExpr(LitBool(true)), DefaultNumber.NUInt)
    )

  def packZ3: Doc = text("Real")
}

case object TyInt extends PrimType {
  def absTopPVal: AbsPValue = DefaultNumber.Top

  def concValList: List[ConcVal] =
    // it is really hard to imagine how to represent this in a constraint solver
    // Maybe full abstraction is *ultimately* the right way to go
    List(
      PredicateVal(
        "x", TyNum,
        BiOpExpr(VarExpr("x"), GreaterEq, LitExpr(LitInt(0))),
        DefaultNumber.UInt
      )
    )

  def packZ3: Doc = text("Int")
}

case object TyVoid extends Type {
  def absTopPVal: AbsPValue = DefaultUndef.Top

  def concValList: List[ConcVal] = List()

  def packZ3: Doc = text("Void") // this doesn't make much sense
}

object getPtype {
  def apply(pval: PValue): PrimType = pval match {
    case _: Num => TyNum
  }
}
