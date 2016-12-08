package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.PValue

import scala.collection.immutable.HashMap

case class Interface(
  name: String,
  kind: InterfaceKind,
  constants: HashMap[String, PValue],
  operations: HashMap[String, Operation]
)

sealed trait InterfaceKind

case object ECMAScriptInterface extends InterfaceKind