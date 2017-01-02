package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsObject, AbsState, AbsValue, DefaultNull }
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.util.Address
import kr.ac.kaist.safe.xwidl.dafny.{ Dafny, Pack, Verified }
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
  private def genSelMode(absValLists: List[(List[AbsValue], String)]): List[List[(AbsValue, String)]] = {
    absValLists.foldRight(List(List[(AbsValue, String)]()))({
      case ((vals, name), lss) => {
        lss.flatMap(ls => vals.map(v => (v, name) :: ls))
      }
    })
  }

  def call(dafny: Dafny, st: AbsState, selfObj: AbsObject,
    selfIface: Interface, args: List[AbsValue]): (AbsValue, AbsObject) = {
    // TODO 1. check the prerequisite

    val absValLists = ensures.freeVars.map({
      case "ret" => (retTy.absValList, "ret")
      case s if s.startsWith("old_this.") => {
        val attr = s.stripPrefix("old_this.")
        (List(selfObj.Get(s, st.heap)), s)
      }
      case s if s.startsWith("this.") => {
        val attr = s.stripPrefix("this.")
        selfIface.getAttrType(attr) match {
          case Some(ty) => (ty.absValList, s)
          case None => return (DefaultNull.Top, selfObj)
        }
      }
      // TODO: args
      case s => return (DefaultNull.Top, selfObj)
    }).toList

    val selModeStream = genSelMode(absValLists)
    for (s <- selModeStream) {
      val ensuresClosed = s.foldLeft(ensures)({
        case (e, (v, name)) => {
          e.subst(name, v)
        }
      })

      dafny.assert(ensuresClosed) match {
        case Verified => {
          val updatedProps = s.filter({ case (_, name) => name.startsWith("this.") })
          val selfObj2 = updatedProps.foldLeft(selfObj)({
            case (selfObj, (propVal, propName)) => {
              val prop = propName.stripPrefix("this.")
              val (selfObj2, _) = selfObj.Put(AbsString(prop), propVal, true, st.heap) // TODO: exception
              selfObj2
            }
          })
          s.find({ case (_, name) => name == "ret" }) match {
            case Some((retVal, _)) => return (retVal, selfObj2)
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
