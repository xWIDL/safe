package kr.ac.kaist.safe.xwidl

sealed trait Expr

case class IfThenElseExpr(cond: Expr,
                          thenBranch: Expr,
                          elseBranch: Expr
) extends Expr

case class BiOpExpr(le: Expr, op: BiOp, re: Expr) extends Expr
case class VarExpr(name: String) extends Expr

sealed trait BiOp

case object EqOp extends BiOp
case object GreaterThan extends BiOp
case object GreaterEq extends BiOp
case object LessEq extends BiOp
case object And extends BiOp
