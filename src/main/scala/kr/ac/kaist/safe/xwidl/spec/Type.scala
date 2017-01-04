package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.xwidl.solver.PackZ3
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Type extends Pack with PackZ3 {
  val absTopPVal: AbsPValue

  val absTopVal: AbsValue = AbsValue(absTopPVal)

  def concValList: List[ConcVal]
}

sealed trait PrimType extends Type

case object TyNum extends PrimType {
  val absTopPVal: AbsPValue = DefaultNumber.Top

  def pack: Doc = text("real")

  def concValList: List[ConcVal] =
    // it is really hard to imagine how to represent this in a constraint solver
    // Maybe full abstraction is *ultimately* the right way to go
    List(
      PredicateVal("x", TyNum, LitExpr(LitBool(true)), DefaultNumber.NUInt)
    )

  def packZ3: Doc = text("Real")
}

case object TyInt extends PrimType {
  val absTopPVal: AbsPValue = DefaultNumber.Top

  def pack: Doc = text("int")

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
  val absTopPVal: AbsPValue = DefaultUndef.Top

  def pack: Doc = text("void")

  def concValList: List[ConcVal] = List()

  def packZ3: Doc = text("Void") // this doesn't make much sense
}

object getPtype {
  def apply(pval: PValue): PrimType = pval match {
    case _: Num => TyNum
  }
}
