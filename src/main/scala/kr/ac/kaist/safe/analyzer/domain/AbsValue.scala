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

import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.analyzer.models.PredefLoc
import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinGlobal

////////////////////////////////////////////////////////////////////////////////
// concrete value type
////////////////////////////////////////////////////////////////////////////////
abstract class Value

////////////////////////////////////////////////////////////////////////////////
// value abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsValue extends AbsDomain[Value, AbsValue] {
  val pvalue: AbsPValue
  val locset: AbsLoc
  val symbol: AbsSym

  /* substitute locR by locO */
  def subsLoc(locR: Loc, locO: Loc): AbsValue
  /* weakly substitute locR by locO, that is keep locR together */
  def weakSubsLoc(locR: Loc, locO: Loc): AbsValue
  // TODO working but a more simple way exists with modifying getBase
  def getThis(h: AbsHeap): AbsLoc

  def typeCount: Int
  def typeKinds: String
}

trait AbsValueUtil extends AbsDomainUtil[Value, AbsValue] {
  def apply(pvalue: AbsPValue): AbsValue
  def apply(locset: AbsLoc): AbsValue
  def apply(pvalue: AbsPValue, locset: AbsLoc): AbsValue
  def symbolize(s: String): AbsValue
}

////////////////////////////////////////////////////////////////////////////////
// default value abstract domain
////////////////////////////////////////////////////////////////////////////////
object DefaultValue extends AbsValueUtil {
  lazy val Bot: Dom = Dom(AbsPValue.Bot, AbsLoc.Bot)
  lazy val Top: Dom = Dom(AbsPValue.Top, AbsLoc.Top)

  def alpha(value: Value): AbsValue = value match {
    case (pvalue: PValue) => apply(AbsPValue(pvalue))
    case (loc: Loc) => apply(AbsLoc(loc))
  }

  def apply(pvalue: AbsPValue): AbsValue = Bot.copy(pvalue = pvalue)
  def apply(locset: AbsLoc): AbsValue = Bot.copy(locset = locset)
  def apply(pvalue: AbsPValue, locset: AbsLoc): AbsValue = Dom(pvalue, locset)
  def symbolize(s: String): AbsValue = Bot.copy(symbol = Some(s))

  case class Dom(pvalue: AbsPValue, locset: AbsLoc, symbol: Option[String] = None) extends AbsValue {
    def gamma: ConSet[Value] = ConInf() // TODO more precisely

    def isBottom: Boolean = this == Bot
    def isTop: Boolean = this == Top

    def getSingle: ConSingle[Value] = ConMany() // TODO more precisely

    def <=(that: AbsValue): Boolean = {
      val (left, right) = (this, check(that))
      left.pvalue <= right.pvalue &&
        left.locset <= right.locset
    }

    def +(that: AbsValue): AbsValue = {
      val (left, right) = (this, check(that))
      Dom(
        left.pvalue + right.pvalue,
        left.locset + right.locset
      )
    }

    def <>(that: AbsValue): AbsValue = {
      val (left, right) = (this, check(that))
      Dom(
        left.pvalue <> right.pvalue,
        left.locset <> right.locset
      )
    }

    override def toString: String = {
      val pvalStr =
        if (pvalue.isBottom) ""
        else pvalue.toString

      val locSetStr =
        if (locset.isBottom) ""
        else locset.toString

      val symStr = symbol match {
        case Some(s) => s
        case None => ""
      }

      (pvalue.isBottom, locset.isBottom, symbol.isDefined) match {
        case (true, true, false) => "⊥(value)"
        case (true, true, true) => s"sym($symStr)"
        case (true, false, _) => locSetStr
        case (false, true, _) => pvalStr
        case (false, false, _) => s"$pvalStr, $locSetStr"
      }
    }

    def subsLoc(locR: Loc, locO: Loc): AbsValue =
      Dom(this.pvalue, this.locset.subsLoc(locR, locO))

    def weakSubsLoc(locR: Loc, locO: Loc): AbsValue =
      Dom(this.pvalue, this.locset.weakSubsLoc(locR, locO))

    def typeCount: Int = {
      if (this.locset.isBottom)
        pvalue.typeCount
      else
        pvalue.typeCount + 1
    }

    def typeKinds: String = {
      val sb = new StringBuilder()
      sb.append(pvalue.typeKinds)
      if (!this.locset.isBottom) sb.append((if (sb.length > 0) ", " else "") + "Object")
      sb.toString
    }

    def getThis(h: AbsHeap): AbsLoc = {
      val locSet1 = (pvalue.nullval.isBottom, pvalue.undefval.isBottom) match {
        case (true, true) => AbsLoc.Bot
        case _ => AbsLoc(BuiltinGlobal.loc)
      }

      val foundDeclEnvRecord = locset.exists(loc => !h.isObject(loc))

      val locSet2 =
        if (foundDeclEnvRecord) AbsLoc(BuiltinGlobal.loc)
        else AbsLoc.Bot
      val locSet3 = locset.foldLeft(AbsLoc.Bot)((tmpLocSet, loc) => {
        if (h.isObject(loc)) tmpLocSet + loc
        else tmpLocSet
      })

      locSet1 + locSet2 + locSet3
    }
  }
}
