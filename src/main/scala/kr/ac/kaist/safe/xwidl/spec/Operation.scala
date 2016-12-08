package kr.ac.kaist.safe.xwidl.spec

case class Operation(
  name: String,
  args: List[Argument],
  retTy: Type,
  ensures: Expr
)

case class Argument(
  name: String,
  ty: Type
)
