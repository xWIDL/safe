package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.PValue
import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

import scala.collection.immutable.HashMap

case class Interface(
    name: String,
    kind: InterfaceKind,
    constants: HashMap[String, PValue],
    operations: HashMap[String, Operation]
) extends Pack {
  def pack: Doc = {
    text("interface") <+> text(name) <+>
      braces(
        stack(constants.map({ case (cname, pval) => text("const") <+> getPtype(pval).pack <+> pval.pack }).toList)

      )
  }
}

sealed trait InterfaceKind

case object ECMAScriptInterface extends InterfaceKind