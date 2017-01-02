package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsObject, AbsState, AbsValue, DefaultNull }
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.util.Address
import kr.ac.kaist.safe.xwidl.solver.{ Solver, Pack, Verified }
import kr.ac.kaist.safe.xwidl.pprint._

case class OperationException(s: String) extends Exception(s)

case class Operation(
    name: String,
    args: List[Argument] = List(),
    retTy: Type,
    objAddr: Address,
    requires: Expr = LitExpr(LitBool(true)),
    ensures: Expr = LitExpr(LitBool(true)),
    absSemOpt: Option[(AbsState, List[AbsValue]) => AbsValue] = None
) extends Pack {
  def pack: Doc = {
    text("method") <+> text(name) <>
      parens(splitBy(args.map(_.pack), text(", "))) <>
      (retTy match {
        case TyVoid => nil
        case _ => text(" returns") <+> parens(text("ret:") <+> retTy.pack)
      }) </>
      text("requires") <+> requires.pack </>
      text("ensures") <+> ensures.pack
  }

  // TODO: meet cases?
  private def genSelMode(concValLists: List[(List[ConcVal], String)]): List[List[(ConcVal, String)]] = {
    concValLists.foldRight(List(List[(ConcVal, String)]()))({
      case ((vals, name), lss) => {
        lss.flatMap(ls => vals.map(v => (v, name) :: ls))
      }
    })
  }

  def call(dafny: Solver, st: AbsState, selfObj: AbsObject,
    selfIface: Interface, args: List[AbsValue]): (AbsValue, AbsObject) = {
    // TODO 1. check the prerequisite

    val concValLists: List[Option[(List[ConcVal], String)]] = ensures.freeVars.map({
      case "ret" => Some(retTy.concValList, "ret")
      case s if s.startsWith("old_this.") => {
        val attr = s.stripPrefix("old_this.")
        Some(List(PreciseVal(selfObj.Get(attr, st.heap))), s)
      }
      case s if s.startsWith("this.") => {
        val attr = s.stripPrefix("this.")
        selfIface.getAttrType(attr) match {
          case Some(ty) => Some(ty.concValList, s)
          case None => None
        }
      }
      // TODO: args
      case _ => None
    }).toList

    if (!concValLists.forall(_.isDefined)) {
      return (DefaultNull.Top, selfObj)
    }

    val selModeStream = genSelMode(concValLists.flatten)
    for (s <- selModeStream) {
      val ensuresClosed = s.foldLeft(ensures)({
        case (e, (concVal, y)) => {
          concVal match {
            case PredicateVal(x, ty, constraint, _) => {
              ExistsExpr(x, ty, BiOpExpr(e.subst(y, VarExpr(x)), And, constraint)) // TODO: alpha conversion
            }
            case PreciseVal(v) => e.subst(y, LitExpr(LitAbs(v)))
          }
        }
      })

      dafny.assert(ensuresClosed) match {
        case Verified => {
          val updatedProps = s.filter({ case (_, name) => name.startsWith("this.") })
          val selfObj2 = updatedProps.foldLeft(selfObj)({
            case (selfObj, (propVal, propName)) => {
              val prop = propName.stripPrefix("this.")
              val (selfObj2, _) = selfObj.Put(AbsString(prop), propVal.alpha, true, st.heap) // TODO: exception
              selfObj2
            }
          })
          s.find({ case (_, name) => name == "ret" }) match {
            case Some((retVal, _)) => return (retVal.alpha, selfObj2)
            case None => return (retTy.absTopVal, selfObj2)
          }
        }
        case _ => ()
      }
    }

    (DefaultNull.Top, selfObj)
  }
}

case class Argument(
    name: String,
    ty: Type
) extends Pack {
  def pack: Doc = text(name) <> text(":") <+> ty.pack
}

// FIXME: variadic functions
