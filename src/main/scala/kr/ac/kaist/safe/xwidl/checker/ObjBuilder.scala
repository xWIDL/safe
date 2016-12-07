package kr.ac.kaist.safe.xwidl.checker

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.xwidl.spec.{ ECMAScriptInterface, Interface, PrimInt }

import scala.collection.immutable.HashMap

object ObjBuilder {
  def buildObj(interface: Interface): ObjModel = {
    ObjModel(
      interface.name,
      interface.constants.map({ case (name, pVal) => NormalProp(name, PrimModel(DefaultPValue(pVal)), F, F, F) }).toList
    )
  }

  def mathInterface: Interface = new Interface(
    "ECMAScriptMath", ECMAScriptInterface,
    HashMap("E" -> Num(2.7182818284590452354)),
    HashMap()
  )
}

// Some examples...

