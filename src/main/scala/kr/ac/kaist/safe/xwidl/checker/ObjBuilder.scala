package kr.ac.kaist.safe.xwidl.checker

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.xwidl.spec.Interface

class ObjBuilder {
  def buildObj(interface: Interface): ObjModel = {
    ObjModel(
      interface.name,
      interface.constants.map({ case (name, pVal) => NormalProp(name, PrimModel(DefaultPValue(pVal)), F, F, F) }).toList
    )
  }
}

