package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsState, AbsValue }
import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

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
}

case class Argument(
    name: String,
    ty: Type
) extends Pack {
  def pack: Doc = text(name) <> text(":") <+> ty.pack
}

// FIXME: variadic functions
