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

import kr.ac.kaist.safe.xwidl.pprint.{ Doc, text }

////////////////////////////////////////////////////////////////////////////////
// concrete null type
////////////////////////////////////////////////////////////////////////////////
sealed abstract class Null extends PValue
case object Null extends Null {
  def pack: Doc = throw new Error("Not implemented")
}

////////////////////////////////////////////////////////////////////////////////
// null abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsNull extends AbsDomain[Null, AbsNull] {
  def ===(that: AbsNull): AbsBool
}

trait AbsNullUtil extends AbsDomainUtil[Null, AbsNull]
