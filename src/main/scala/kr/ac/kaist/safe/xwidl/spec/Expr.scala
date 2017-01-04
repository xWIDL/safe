package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.AbsValue
import kr.ac.kaist.safe.nodes.cfg.CFGExpr
import kr.ac.kaist.safe.xwidl.solver.{Pack, PackZ3}
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Expr extends Pack with PackZ3 {
  def freeVars: Set[String]

  def subst(name: String, v: Expr): Expr

  def rewrite: CFGExpr
  /*
   * We need to process qualifiers etc. Maybe implication and more exotic thing in the future
   */
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

  def subst(name: String, v: Expr): Expr =
    IfThenElseExpr(cond.subst(name, v), thenBranch.subst(name, v), elseBranch.subst(name, v))

  def packZ3: Doc =
    parens(text("if") <+> cond.packZ3 <+> thenBranch.packZ3 <+> elseBranch.packZ3)
}

case class BiOpExpr(le: Expr, op: BiOp, re: Expr) extends Expr {
  def pack: Doc = le.pack <+> op.pack <+> re.pack

  override def freeVars: Set[String] = le.freeVars union re.freeVars

  def subst(name: String, v: Expr): Expr =
    BiOpExpr(le.subst(name, v), op, re.subst(name, v))

  def packZ3: Doc =
    parens(op.packZ3 <+> le.packZ3 <+> re.packZ3)
}
case class VarExpr(name: String) extends Expr {
  def pack: Doc = text(name)

  override def freeVars: Set[String] = Set(name)

  def subst(x: String, v: Expr): Expr =
    if (x == name) {
      v
    } else {
      this
    }

  def packZ3: Doc = text(name)
}

case class ForallExpr(x: String, ty: Type, e: Expr) extends Expr {
  def pack: Doc = text("forall " + x + " :: ") <> parens(e.pack)
  def freeVars: Set[String] = e.freeVars - x
  def subst(y: String, v: Expr): Expr =
    if (x == y) {
      this
    } else {
      ForallExpr(x, ty, e.subst(y, v))
    }

  def packZ3: Doc = parens(text("forall") <+> parens(parens(text(x) <+> ty.packZ3)) <+> e.packZ3)
}

case class ExistsExpr(x: String, ty: Type, e: Expr) extends Expr {
  def pack: Doc = text("exists " + x + " :: ") <> parens(e.pack)
  def freeVars: Set[String] = e.freeVars - x
  def subst(y: String, v: Expr): Expr =
    if (x == y) {
      this
    } else {
      ExistsExpr(x, ty, e.subst(y, v))
    }
  def packZ3: Doc = parens(text("exists") <+> parens(parens(text(x) <+> ty.packZ3)) <+> e.packZ3)
}

case class LitExpr(lit: Literal) extends Expr {
  def pack: Doc = lit.pack

  override def freeVars: Set[String] = Set()

  def subst(x: String, v: Expr): Expr = this

  def packZ3: Doc = lit.packZ3
}

sealed trait BiOp extends Pack {
  def pack: Doc = text(this.toString)
  def packZ3: Doc = text(this.toString)
}

case object EqOp extends BiOp {
  override def toString: String = "=="
  override def packZ3: Doc = text("=")
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

  override def packZ3: Doc = text("and")
}
case object Minus extends BiOp {
  override def toString: String = "-"
}
case object Plus extends BiOp {
  override def toString: String = "+"
}

sealed trait Literal extends Pack {
  def packZ3: Doc = pack
}

case class LitInt(i: Int) extends Literal {
  def pack: Doc = text(i.toString)
}
case class LitNum(i: Double) extends Literal {
  def pack: Doc = text(i.toString)
}
case class LitBool(b: Boolean) extends Literal {
  def pack: Doc = text(b.toString)
}
case class LitAbs(a: AbsValue) extends Literal {
  def pack: Doc = text(a.toString)
}
