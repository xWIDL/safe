package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Type extends Pack {
  val absTopPVal: AbsPValue

  val absTopVal: AbsValue = AbsValue(absTopPVal)

  def absValList: List[AbsValue]
}

sealed trait PrimType extends Type

case object TyNum extends PrimType {
  val absTopPVal: AbsPValue = DefaultNumber.Top

  def pack: Doc = text("real")

  def absValList: List[AbsValue] =
    // it is really hard to imagine how to represent this in a constraint solver
    // Maybe full abstraction is *ultimately* the right way to go
    List(DefaultNumber.Inf, DefaultNumber.UInt, DefaultNumber.NUInt)
}

case object TyVoid extends Type {
  val absTopPVal: AbsPValue = DefaultUndef.Top

  def pack: Doc = text("void")

  def absValList: List[AbsValue] = List()
}

object getPtype {
  def apply(pval: PValue): PrimType = pval match {
    case _: Num => TyNum
  }
}
