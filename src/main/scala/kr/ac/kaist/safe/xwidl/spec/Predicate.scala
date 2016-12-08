package kr.ac.kaist.safe.xwidl.spec

case class Predicate (
                     binder: String,
                     binderTy: Type,
                     body: Expr
                     )

case object Predicate {
  def apply(binder: String, binderTy: Type, body: Expr): Predicate = new Predicate(binder, binderTy, body)
}