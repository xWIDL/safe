package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Type extends Pack {
  val absVal: AbsValue
}

sealed trait PrimType extends Type

case object TyNum extends PrimType {
  val absVal: AbsValue = AbsValue(AbsNumber.Top)

  def pack: Doc = text("double")
}

object getPtype {
  def apply(pval: PValue): PrimType = pval match {
    case _: Num => TyNum
  }
}
