package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.PValue
import kr.ac.kaist.safe.util.Address
import kr.ac.kaist.safe.xwidl.dafny.Pack
import kr.ac.kaist.safe.xwidl.pprint._

import scala.collection.immutable.HashMap

case class Interface(
    name: String,
    kind: InterfaceKind,
    constants: HashMap[String, PValue] = HashMap(),
    instanceAddr: Address,
    attrs: HashMap[String, (Type, PValue)] = HashMap(),
    /* NOTE: Constants will not be packed;
       instead, it will be directly inlined when needed.
       See ObjBuilder. */

    operations: HashMap[String, Operation] = HashMap()
) extends Pack {
  def pack: Doc = {
    text("class") <+> text(name) <+>
      braces(stack(operations.values.map(_.pack).toList))
  }

  def getAttrType(attr: String): Option[Type] =
    this.attrs.get(attr) match {
      case Some((ty, _)) => Some(ty)
      case _ => None
    }
}

sealed trait InterfaceKind

case object ECMAScriptInterface extends InterfaceKind
case object WebAPIInterface extends InterfaceKind