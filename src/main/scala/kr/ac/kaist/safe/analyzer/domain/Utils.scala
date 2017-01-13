/**
 * *****************************************************************************
 * Copyright (c) 2016, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */

package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.nodes.cfg.CFG
import scala.collection.immutable.HashSet

object Utils {
  def register(
    absUndef: AbsUndefUtil,
    absNull: AbsNullUtil,
    absBool: AbsBoolUtil,
    absNumber: AbsNumberUtil,
    absString: AbsStringUtil,
    absLoc: AbsLocUtil
  ): Unit = {
    AbsUndef = absUndef
    AbsNull = absNull
    AbsBool = absBool
    AbsNumber = absNumber
    AbsString = absString
    AbsLoc = absLoc
  }

  // primitive values
  var AbsUndef: AbsUndefUtil = _
  var AbsNull: AbsNullUtil = _
  var AbsBool: AbsBoolUtil = _
  var AbsNumber: AbsNumberUtil = _
  var AbsString: AbsStringUtil = _
  var AbsPValue: AbsPValueUtil = DefaultPValue

  // location
  var AbsLoc: AbsLocUtil = _

  var AbsSym: AbsSymUtil = DefaultSym(Set()) // XXX: So, what does an empty default symbol set mean?

  // value
  var AbsValue: AbsValueUtil = DefaultValue

  // data property
  var AbsDataProp: AbsDataPropUtil = DefaultDataProp

  // descriptor
  var AbsDesc: AbsDescUtil = DefaultDesc

  // absent value for parital map
  var AbsAbsent: AbsAbsentUtil = DefaultAbsent

  // execution context
  var AbsBinding: AbsBindingUtil = DefaultBinding
  var AbsDecEnvRec: AbsDecEnvRecUtil = DefaultDecEnvRec
  var AbsGlobalEnvRec: AbsGlobalEnvRecUtil = DefaultGlobalEnvRec
  var AbsEnvRec: AbsEnvRecUtil = DefaultEnvRec
  var AbsLexEnv: AbsLexEnvUtil = DefaultLexEnv
  var AbsContext: AbsContextUtil = DefaultContext

  // object
  var AbsObject: AbsObjectUtil = DefaultObject

  // heap
  var AbsHeap: AbsHeapUtil = DefaultHeap

  var AbsPredHeap: AbsPredHeapUtil = DefaultPredHeap

  // state
  var AbsState: AbsStateUtil = DefaultState

  // concrete domains
  def ConSingle[T]: ConSingleUtil[T] = ConSingleUtil[T]
  def ConSet[T]: ConSetUtil[T] = ConSetUtil[T]
}
