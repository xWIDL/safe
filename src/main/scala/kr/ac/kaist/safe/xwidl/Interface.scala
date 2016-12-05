package kr.ac.kaist.safe.xwidl

import kr.ac.kaist.safe.analyzer.domain.PValue

import scala.collection.immutable.HashMap

class Interface (val name: String,
                 val kind: InterfaceKind,
                 val constants: HashMap[String, PValue],
                 val operations: HashMap[String, Operation]
) {

}

sealed trait InterfaceKind

case class ECMAScriptInterface() extends InterfaceKind