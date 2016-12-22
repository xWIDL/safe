package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Expr extends Pack

case class IfThenElseExpr(
    cond: Expr,
    thenBranch: Expr,
    elseBranch: Expr
) extends Expr {

  def pack: Doc =
    text("if") <> parens(cond.pack) <+>
      text("then") <+> braces(thenBranch.pack) <+> text("else") <+> braces(elseBranch.pack)
}

case class BiOpExpr(le: Expr, op: BiOp, re: Expr) extends Expr {
  def pack: Doc = le.pack <+> op.pack <+> re.pack
}
case class VarExpr(name: String) extends Expr {
  def pack: Doc = text(name)
}

case class LitExpr(lit: Literal) extends Expr {
  def pack: Doc = lit.pack
}

sealed trait BiOp extends Pack {
  def pack: Doc = text(this.toString)
}

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
case object Minus extends BiOp {
  override def toString: String = "-"
}
case object Plus extends BiOp {
  override def toString: String = "+"
}

sealed trait Literal extends Pack

case class PrimInt(i: Int) extends Literal {
  def pack: Doc = text(i.toString)
}

case class PrimBool(b: Boolean) extends Literal {
  def pack: Doc = text(b.toString)
}

