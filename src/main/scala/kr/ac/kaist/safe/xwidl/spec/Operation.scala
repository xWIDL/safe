package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.Semantics
import kr.ac.kaist.safe.analyzer.domain.DefaultBool.True
import kr.ac.kaist.safe.analyzer.domain.{ AbsObject, AbsState, AbsValue, DefaultBool, DefaultNull }
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.util.Address
import kr.ac.kaist.safe.xwidl.solver.{ Pack, Solver, Verified }
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

    val boundAbsVals: List[Option[(AbsValue, String)]] = requires.freeVars.map({
      case s if s.startsWith("this.") => {
        val attr = s.stripPrefix("this.")
        Some(selfObj.Get(attr, st.heap), s)
      }
      case _ => None
    }).toList

    if (!boundAbsVals.forall(_.isDefined)) {
      return (DefaultNull.Top, selfObj)
    }

    val requiresClosed = boundAbsVals.flatten.foldLeft(requires)({
      case (e, (absVal, y)) => e.subst(y, LitExpr(LitAbs(absVal)))
    })

    requiresClosed.eval(st) match {
      case Some(v) if DefaultBool.True <= v.pvalue.boolval => {
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
          dafny.assert(ensures.substConcVals(s)) match {
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
      case _ => {
        println("Something is wrong with spec")
        (DefaultNull.Top, selfObj)
      }
    }
  }
}

case class Argument(
    name: String,
    ty: Type
) extends Pack {
  def pack: Doc = text(name) <> text(":") <+> ty.pack
}

// FIXME: variadic functions
