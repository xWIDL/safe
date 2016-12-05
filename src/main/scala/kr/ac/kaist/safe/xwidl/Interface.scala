package kr.ac.kaist.safe.xwidl

import kr.ac.kaist.safe.analyzer.domain.PValue

import scala.collection.immutable.HashMap

class Interface(
    name: String,
    kind: InterfaceKind,
    constants: HashMap[String, PValue],
    operations: HashMap[String, Operation]
) {

}

sealed trait InterfaceKind

case object ECMAScriptInterface extends InterfaceKind