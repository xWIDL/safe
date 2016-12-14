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
// concrete boolean type
////////////////////////////////////////////////////////////////////////////////
case class Bool(b: Boolean) extends PValue {
  def pack: Doc = text(b.toString)
}

////////////////////////////////////////////////////////////////////////////////
// boolean abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsBool extends AbsDomain[Bool, AbsBool] {
  def ===(that: AbsBool): AbsBool
  def negate: AbsBool
  def &&(that: AbsBool): AbsBool
  def ||(that: AbsBool): AbsBool
  def xor(that: AbsBool): AbsBool

  def toAbsNumber: AbsNumber
  def toAbsString: AbsString

  def map[T <: Domain[T]](
    thenV: => T,
    elseV: => T
  )(implicit util: DomainUtil[T]): T

  // TODO remove after when product domains are possible
  def map[T <: Domain[T], U <: Domain[U]](
    thenV: => (T, U),
    elseV: => (T, U)
  )(implicit util: (DomainUtil[T], DomainUtil[U])): (T, U)
}

trait AbsBoolUtil extends AbsDomainUtil[Bool, AbsBool] {
  // abstraction from true
  val True: AbsBool

  // abstraction from false
  val False: AbsBool
}
