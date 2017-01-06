package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.Helper
import kr.ac.kaist.safe.analyzer.domain.DefaultNumber.UIntConst
import kr.ac.kaist.safe.analyzer.domain.{ AbsState, AbsValue, DefaultBool }
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.util.EJSOp
import kr.ac.kaist.safe.xwidl.solver.PackZ3
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Expr extends PackZ3 {
  def freeVars: Set[String]

  def subst(name: String, v: Expr): Expr

  def eval(st: AbsState): Option[Expr] // partial evaluation (full eval is the special case when it is AbsExpr(abs))
  /*
   * We need to process qualifiers etc. Maybe implication and more exotic thing in the future
   */

  def substConcVal(concVal: ConcVal, y: String): Expr = concVal match {
    case PredicateVal(x, ty, constraint, _) => {
      ExistsExpr(x, ty, BiOpExpr(this.subst(y, VarExpr(x)), And, constraint)) // TODO: alpha conversion
    }
    case PreciseVal(v) => this.subst(y, AbsValExpr(v))
  }

  def substConcVals(s: List[(ConcVal, String)]): Expr = {
    s.foldLeft(this)({ case (e, (concVal, y)) => e.substConcVal(concVal, y) })
  }

  def <||>(e2: Expr): Expr = BiOpExpr(this, Or, e2)
  def <&&>(e2: Expr): Expr = BiOpExpr(this, And, e2)
}

case class IfThenElseExpr(
    cond: Expr,
    thenBranch: Expr,
    elseBranch: Expr
) extends Expr {

  def freeVars: Set[String] =
    cond.freeVars union thenBranch.freeVars union elseBranch.freeVars

  def subst(name: String, v: Expr): Expr =
    IfThenElseExpr(cond.subst(name, v), thenBranch.subst(name, v), elseBranch.subst(name, v))

  def packZ3: Doc =
    parens(text("if") <+> cond.packZ3 <+> thenBranch.packZ3 <+> elseBranch.packZ3)

  def eval(st: AbsState): Option[Expr] = {
    lazy val thenBranchVal = thenBranch.eval(st)
    lazy val elseBranchVal = elseBranch.eval(st)
    cond.eval(st) match {
      case Some(AbsValExpr(condVal)) => condVal.pvalue.boolval match {
        case DefaultBool.True => thenBranchVal
        case DefaultBool.False => elseBranchVal
        case DefaultBool.Top =>
          for { a <- thenBranchVal; b <- elseBranchVal } yield (a, b) match {
            case (AbsValExpr(a1), AbsValExpr(a2)) => AbsValExpr(a1 + a2)
            case (e1, e2) => IfThenElseExpr(AbsValExpr(condVal), e1, e2)
          }
        case DefaultBool.Bot => None
      }
      case Some(e) => for { a <- thenBranchVal; b <- elseBranchVal } yield (a, b) match {
        case (AbsValExpr(a1), AbsValExpr(a2)) => AbsValExpr(a1 + a2)
        case (e1, e2) => IfThenElseExpr(e, e1, e2)
      }
      case None => None
    }
  }
}

case class BiOpExpr(le: Expr, op: BiOp, re: Expr) extends Expr {

  override def freeVars: Set[String] = le.freeVars union re.freeVars

  def subst(name: String, v: Expr): Expr =
    BiOpExpr(le.subst(name, v), op, re.subst(name, v))

  def packZ3: Doc =
    parens(op.packZ3 <+> le.packZ3 <+> re.packZ3)

  private def biOpEvalHelper(le: Expr, re: Expr,
    fe: (Expr, Expr) => Expr,
    fv: (AbsValue, AbsValue) => AbsValue): Expr = {
    (le, re) match {
      case (AbsValExpr(la), AbsValExpr(ra)) => AbsValExpr(fv(la, ra))
      case _ => fe(le, re)
    }
  }

  def eval(st: AbsState): Option[Expr] = {
    for { leVal <- le.eval(st); reVal <- re.eval(st) } yield biOpEvalHelper(leVal, reVal, op.toBiExpr, op.toBopHelper)
  }
}
case class VarExpr(name: String) extends Expr {
  def freeVars: Set[String] = Set(name)

  def subst(x: String, v: Expr): Expr = if (x == name) { v } else { this }

  def packZ3: Doc = text(name)

  def eval(st: AbsState): Option[Expr] = Some(this)
}

case class ForallExpr(x: String, ty: Type, e: Expr) extends Expr {
  def freeVars: Set[String] = e.freeVars - x
  def subst(y: String, v: Expr): Expr =
    if (x == y) {
      this
    } else {
      ForallExpr(x, ty, e.subst(y, v))
    }

  def packZ3: Doc = parens(text("forall") <+> parens(parens(text(x) <+> ty.packZ3)) <+> e.packZ3)

  def eval(st: AbsState): Option[Expr] = {
    e.subst(x, AbsValExpr(ty.absTopVal)).eval(st)
  }
}

case class ExistsExpr(x: String, ty: Type, e: Expr) extends Expr {
  def freeVars: Set[String] = e.freeVars - x
  def subst(y: String, v: Expr): Expr =
    if (x == y) {
      this
    } else {
      ExistsExpr(x, ty, e.subst(y, v))
    }
  def packZ3: Doc = parens(text("exists") <+> parens(parens(text(x) <+> ty.packZ3)) <+> e.packZ3)

  def eval(st: AbsState): Option[Expr] = e.eval(st).map(ExistsExpr(x, ty, _))
  // partially evaluate the body
}

case class LitExpr(lit: Literal) extends Expr {

  override def freeVars: Set[String] = Set()

  def subst(x: String, v: Expr): Expr = this

  def packZ3: Doc = lit.packZ3

  def eval(st: AbsState): Option[Expr] = Some(AbsValExpr(lit.alpha))
}

case class AbsValExpr(abs: AbsValue) extends Expr {
  def freeVars: Set[String] = Set() // really?
  def subst(x: String, v: Expr): Expr = this
  def packZ3: Doc = text(abs.toString)
  def eval(st: AbsState): Option[Expr] = {
    abs.symbol match {
      case Some(s) => Some(VarExpr(s))
      case None => Some(this)
    }
  }
}

sealed trait BiOp {
  def packZ3: Doc = text(this.toString)
  def toEJSop: EJSOp = EJSOp(this.toString)
  def toBiExpr(e1: Expr, e2: Expr): Expr = BiOpExpr(e1, this, e2)
  def toBopHelper: (AbsValue, AbsValue) => AbsValue = this match {
    case EqOp => Helper.bopPlus
    case GreaterThan => Helper.bopGreater
    case GreaterEq => Helper.bopGreaterEq
    case LessEq => Helper.bopLess
    case And => Helper.bopAnd
    case Minus => Helper.bopMinus
    case Plus => Helper.bopPlus
  }
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
case object Or extends BiOp {
  override def toString: String = "||"
  override def packZ3: Doc = text("or")
}
case object Minus extends BiOp {
  override def toString: String = "-"
}
case object Plus extends BiOp {
  override def toString: String = "+"
}
sealed trait Literal extends PackZ3 {
  def alpha: AbsValue
}

case class LitInt(i: Int) extends Literal {
  def packZ3: Doc = text(i.toString)
  def alpha: AbsValue = UIntConst(i)
}
case class LitNum(i: Double) extends Literal {
  def packZ3: Doc = text(i.toString)
  def alpha: AbsValue = AbsNumber(i)
}
case class LitBool(b: Boolean) extends Literal {
  def packZ3: Doc = text(b.toString)
  def alpha: AbsValue = AbsBool(b)
}
