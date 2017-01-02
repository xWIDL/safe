package kr.ac.kaist.safe.xwidl.solver

import kr.ac.kaist.safe.xwidl.pprint.Doc

trait Pack {
  def pack: Doc
}

trait PackZ3 {
  def packZ3: Doc
}
