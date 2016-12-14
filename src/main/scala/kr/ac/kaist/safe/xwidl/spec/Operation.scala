package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsState, AbsValue }
import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

case class Operation(
    name: String,
    args: List[Argument],
    retTy: Type,
    ensures: Expr,
    absSemOpt: Option[(AbsState, List[AbsValue]) => AbsValue]
) extends Pack {
  def pack: Doc = {
    text("method") <+> text(name) <>
      parens(splitBy(args.map(_.pack), text(", ")))
    // FIXME: if return type is not void, we need something here
  }
}

case class Argument(
    name: String,
    ty: Type
) extends Pack {
  def pack: Doc = text(name) <> text(":") <+> ty.pack
}

// FIXME: variadic functions
