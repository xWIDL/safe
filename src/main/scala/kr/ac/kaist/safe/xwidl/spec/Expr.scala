package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.Helper
import kr.ac.kaist.safe.analyzer.domain.DefaultNumber.UIntConst
import kr.ac.kaist.safe.analyzer.domain.{ AbsBool, AbsNumber, AbsState, AbsValue, DefaultBool, Num }
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.util.EJSOp
import kr.ac.kaist.safe.xwidl.solver.{ Pack, PackZ3 }
import kr.ac.kaist.safe.xwidl.pprint._

sealed trait Expr extends Pack with PackZ3 {
  def freeVars: Set[String]

  def subst(name: String, v: Expr): Expr

  def eval(st: AbsState): Option[AbsValue]
  /*
   * We need to process qualifiers etc. Maybe implication and more exotic thing in the future
   */

  def substConcVal(concVal: ConcVal, y: String): Expr = concVal match {
    case PredicateVal(x, ty, constraint, _) => {
      ExistsExpr(x, ty, BiOpExpr(this.subst(y, VarExpr(x)), And, constraint)) // TODO: alpha conversion
    }
    case PreciseVal(v) => this.subst(y, LitExpr(LitAbs(v)))
  }

  def substConcVals(s: List[(ConcVal, String)]): Expr = {
    s.foldLeft(this)({ case (e, (concVal, y)) => e.substConcVal(concVal, y) })
  }
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

  def eval(st: AbsState): Option[AbsValue] = {
    lazy val thenBranchVal = thenBranch.eval(st)
    lazy val elseBranchVal = elseBranch.eval(st)
    cond.eval(st) match {
      case Some(condVal) => condVal.pvalue.boolval match {
        case DefaultBool.True => thenBranchVal
        case DefaultBool.False => elseBranchVal
        case DefaultBool.Top => for { a <- thenBranchVal; b <- elseBranchVal } yield a + b
        case DefaultBool.Bot => None
      }
      case None => None
    }
  }
}

case class BiOpExpr(le: Expr, op: BiOp, re: Expr) extends Expr {
  def pack: Doc = le.pack <+> op.pack <+> re.pack

  override def freeVars: Set[String] = le.freeVars union re.freeVars

  def subst(name: String, v: Expr): Expr =
    BiOpExpr(le.subst(name, v), op, re.subst(name, v))

  def packZ3: Doc =
    parens(op.packZ3 <+> le.packZ3 <+> re.packZ3)

  def eval(st: AbsState): Option[AbsValue] = {
    for { leVal <- le.eval(st); reVal <- re.eval(st) } yield {
      op match {
        case EqOp => Helper.bopPlus(leVal, reVal)
        case GreaterThan => Helper.bopGreater(leVal, reVal)
        case GreaterEq => Helper.bopGreaterEq(leVal, reVal)
        case LessEq => Helper.bopLess(leVal, reVal)
        case And => Helper.bopAnd(leVal, reVal)
        case Minus => Helper.bopMinus(leVal, reVal)
        case Plus => Helper.bopPlus(leVal, reVal)
      }
    }
  }
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

  def eval(st: AbsState): Option[AbsValue] = None // this should be instantiated already
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

  def eval(st: AbsState): Option[AbsValue] = {
    e.subst(x, LitExpr(LitAbs(ty.absTopVal))).eval(st)
  }
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

  def eval(st: AbsState): Option[AbsValue] = {
    ty.concValList.map(concVal => {
      e.substConcVal(concVal, x).eval(st)
    }).find(_.isDefined).flatten
  }
}

case class LitExpr(lit: Literal) extends Expr {
  def pack: Doc = lit.pack

  override def freeVars: Set[String] = Set()

  def subst(x: String, v: Expr): Expr = this

  def packZ3: Doc = lit.packZ3

  def eval(st: AbsState): Option[AbsValue] = Some(lit.alpha)
}

sealed trait BiOp extends Pack {
  def pack: Doc = text(this.toString)
  def packZ3: Doc = text(this.toString)
  def toEJSop: EJSOp = EJSOp(this.toString)
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
  def alpha: AbsValue
}

case class LitInt(i: Int) extends Literal {
  def pack: Doc = text(i.toString)
  def alpha: AbsValue = UIntConst(i)
}
case class LitNum(i: Double) extends Literal {
  def pack: Doc = text(i.toString)
  def alpha: AbsValue = AbsNumber(i)
}
case class LitBool(b: Boolean) extends Literal {
  def pack: Doc = text(b.toString)
  def alpha: AbsValue = AbsBool(b)
}
case class LitAbs(a: AbsValue) extends Literal {
  def pack: Doc = text(a.toString)
  def alpha: AbsValue = a
}
