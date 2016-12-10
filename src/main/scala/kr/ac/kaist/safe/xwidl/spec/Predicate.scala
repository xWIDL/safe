package kr.ac.kaist.safe.xwidl.spec

case class Predicate(
  binder: String,
  binderTy: Type,
  body: Expr
)
