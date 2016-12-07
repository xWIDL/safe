package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.PValue

import scala.collection.immutable.HashMap

class Interface(
    val name: String,
    kind: InterfaceKind,
    val constants: HashMap[String, PValue],
    operations: HashMap[String, Operation]
) {

}

sealed trait InterfaceKind

case object ECMAScriptInterface extends InterfaceKind