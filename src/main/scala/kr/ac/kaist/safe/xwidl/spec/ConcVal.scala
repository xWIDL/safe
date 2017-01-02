package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.AbsValue

sealed trait ConcVal {
  val alpha: AbsValue
}

case class PredicateVal(
    binder: String,
    binderTy: Type,
    body: Expr,
    v: AbsValue
) extends ConcVal {
  val alpha: AbsValue = v
}

case class PreciseVal(v: AbsValue) extends ConcVal {
  val alpha: AbsValue = v
}
