package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsState, AbsValue }

case class Operation(
  name: String,
  args: List[Argument],
  retTy: Type,
  ensures: Expr,
  absSemOpt: Option[(AbsState, List[AbsValue]) => AbsValue]
)

case class Argument(
  name: String,
  ty: Type
)

// FIXME: variadic functions
