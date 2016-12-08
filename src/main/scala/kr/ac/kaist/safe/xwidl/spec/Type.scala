package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._

sealed trait Type {
  val absVal: AbsValue
}

sealed trait PrimType extends Type

case object TyNum extends PrimType {
  val absVal: AbsValue = AbsValue(AbsNumber.Top)
}
