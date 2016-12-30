package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsObject, AbsState, AbsValue, DefaultNull }
import kr.ac.kaist.safe.xwidl.dafny.{ Dafny, Pack, Verified }
import kr.ac.kaist.safe.xwidl.pprint._

import scala.collection.JavaConverters._

case class Operation(
    name: String,
    args: List[Argument],
    retTy: Type,
    requires: Expr,
    ensures: Expr,
    absSemOpt: Option[(AbsState, List[AbsValue]) => AbsValue]
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

  def call(dafny: Dafny, st: AbsState, self: AbsObject,
    selfIface: Interface, args: List[AbsValue]): AbsValue = {
    // TODO 1. check the prerequisite

    val absValLists = ensures.freeVars.map({
      case "ret" => (retTy.absValList, "ret")
      case s if s.startsWith("this.") => {
        val attr = s.stripPrefix("this.")
        selfIface.getAttrType(attr) match {
          case Some(ty) => (ty.absValList, s)
          case None => throw new Exception(s"Unknown attribute $attr of ${selfIface.name} interface")
        }
      }
      case s => throw new Exception(s"Unknown free var $s")
    }).toList

    val selModeStream = genSelMode(absValLists)
    for (s <- selModeStream) {
      val ensuresClosed = s.foldLeft(ensures)({
        case (e, (v, name)) => {
          e.subst(name, v)
        }
      })

      dafny.assert(ensuresClosed) match {
        case Verified => s.find({ case (_, name) => name == "ret" }) match {
          case Some((retVal, _)) => return retVal
          case None => return retTy.absTopVal
        }
        case _ => ()
      }
    }

    DefaultNull.Top
  }
}

case class Argument(
    name: String,
    ty: Type
) extends Pack {
  def pack: Doc = text(name) <> text(":") <+> ty.pack
}

// FIXME: variadic functions
