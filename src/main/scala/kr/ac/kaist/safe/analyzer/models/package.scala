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

package kr.ac.kaist.safe.analyzer

import kr.ac.kaist.safe.analyzer.domain.AbsState
import kr.ac.kaist.safe.nodes.cfg.CFGBlock

package object models {
  type SemanticFun = (AbsState, CFGBlock) => (AbsState, AbsState)
  val T = true
  val F = false
}
