package kr.ac.kaist.safe.xwidl.spec

sealed trait Expr

case class IfThenElseExpr(
  cond: Expr,
  thenBranch: Expr,
  elseBranch: Expr
) extends Expr

case class BiOpExpr(le: Expr, op: BiOp, re: Expr) extends Expr
case class VarExpr(name: String) extends Expr
case class LitExpr(lit: Literal) extends Expr

sealed trait BiOp

case object EqOp extends BiOp {
  override def toString: String = "="
}
case object GreaterThan extends BiOp {
  override def toString: String = ">"
}
case object GreaterEq extends BiOp {
  override def toString: String = ">="
}
case object LessEq extends BiOp {
  override def toString: String = "<="
}
case object And extends BiOp {
  override def toString: String = "&&"
}

sealed trait Literal

case class PrimInt(i: Int) extends Literal
case class PrimBool(b: Boolean) extends Literal

