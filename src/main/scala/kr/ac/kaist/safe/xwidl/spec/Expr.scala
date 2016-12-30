package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.AbsValue
import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Expr extends Pack {
  def freeVars: Set[String]

  def subst(name: String, v: AbsValue): Expr

}

case class IfThenElseExpr(
    cond: Expr,
    thenBranch: Expr,
    elseBranch: Expr
) extends Expr {

  def pack: Doc =
    text("if") <+> cond.pack <+> text("then") <+> thenBranch.pack <+> text("else") <+> elseBranch.pack

  def freeVars: Set[String] =
    cond.freeVars union thenBranch.freeVars union elseBranch.freeVars

  def subst(name: String, v: AbsValue): Expr =
    IfThenElseExpr(cond.subst(name, v), thenBranch.subst(name, v), elseBranch.subst(name, v))

}

case class BiOpExpr(le: Expr, op: BiOp, re: Expr) extends Expr {
  def pack: Doc = le.pack <+> op.pack <+> re.pack

  override def freeVars: Set[String] = le.freeVars union re.freeVars

  def subst(name: String, v: AbsValue): Expr =
    BiOpExpr(le.subst(name, v), op, re.subst(name, v))
}
case class VarExpr(name: String) extends Expr {
  def pack: Doc = text(name)

  override def freeVars: Set[String] = Set(name)

  def subst(x: String, v: AbsValue): Expr =
    if (x == name) {
      LitExpr(PrimAbs(v))
    } else {
      this
    }
}

case class LitExpr(lit: Literal) extends Expr {
  def pack: Doc = lit.pack

  override def freeVars: Set[String] = Set()

  def subst(x: String, v: AbsValue): Expr = this
}

sealed trait BiOp extends Pack {
  def pack: Doc = text(this.toString)
}

case object EqOp extends BiOp {
  override def toString: String = "=="
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
case class PrimNum(i: Float) extends Literal {
  def pack: Doc = text(i.toString)
}
case class PrimBool(b: Boolean) extends Literal {
  def pack: Doc = text(b.toString)
}
case class PrimAbs(a: AbsValue) extends Literal {
  def pack: Doc = text(a.toString)
}
