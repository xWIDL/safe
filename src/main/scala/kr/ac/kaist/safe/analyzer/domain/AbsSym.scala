package kr.ac.kaist.safe.analyzer.domain

import kr.ac.kaist.safe.nodes.cfg.BlockId

////////////////////////////////////////////////////////////////////////////////
// concrete location type
////////////////////////////////////////////////////////////////////////////////
case class Sym(s: String, tag: BlockId) extends Value {
  override def toString: String = s"Sym($s@$tag)"
}

////////////////////////////////////////////////////////////////////////////////
// symbol abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsSym extends AbsDomain[Sym, AbsSym] {
  def contains(loc: Sym): Boolean
  def exists(f: Sym => Boolean): Boolean
  def filter(f: Sym => Boolean): AbsSym
  def foreach(f: Sym => Unit): Unit
  def foldLeft[T](initial: T)(f: (T, Sym) => T): T
  def map[T](f: Sym => T): Set[T]
  def isConcrete: Boolean
  def +(loc: Sym): AbsSym
  def -(loc: Sym): AbsSym
  /* substitute locR by locO */
  def subsLoc(locR: Sym, locO: Sym): AbsSym
  /* weakly substitute locR by locO, that is keep locR together */
  def weakSubsLoc(locR: Sym, locO: Sym): AbsSym
}

trait AbsSymUtil extends AbsDomainUtil[Sym, AbsSym]

////////////////////////////////////////////////////////////////////////////////
// default location abstract domain
////////////////////////////////////////////////////////////////////////////////
case class DefaultSym(symSet: Set[Sym]) extends AbsSymUtil {
  case object Top extends Dom
  case class LocSet(set: Set[Sym]) extends Dom
  object LocSet {
    def apply(seq: Sym*): LocSet = LocSet(seq.toSet)
  }
  lazy val Bot: Dom = LocSet()

  def alpha(loc: Sym): AbsSym = LocSet(loc)
  override def alpha(locset: Set[Sym]): AbsSym = LocSet(locset)

  sealed abstract class Dom extends AbsSym {
    def gamma: ConSet[Sym] = this match {
      case Top => ConFin(symSet)
      case LocSet(set) => ConFin(set)
    }

    def isBottom: Boolean = this == Bot
    def isTop: Boolean = this == Top

    def getSingle: ConSingle[Sym] = this match {
      case LocSet(set) if set.size == 0 => ConZero()
      case LocSet(set) if set.size == 1 => ConOne(set.head)
      case _ => ConMany()
    }

    override def toString: String = this match {
      case Top => "Top(symbol)"
      case LocSet(set) if set.size == 0 => "⊥(symbol)"
      case LocSet(set) => set.mkString(", ")
    }

    def <=(that: AbsSym): Boolean = (this, check(that)) match {
      case (_, Top) => true
      case (Top, _) => false
      case (LocSet(lset), LocSet(rset)) => lset subsetOf rset
    }

    def +(that: AbsSym): AbsSym = (this, check(that)) match {
      case (Top, _) | (_, Top) => Top
      case (LocSet(lset), LocSet(rset)) => LocSet(lset ++ rset)
    }

    def <>(that: AbsSym): AbsSym = (this, check(that)) match {
      case (Top, _) => that
      case (_, Top) => this
      case (LocSet(lset), LocSet(rset)) => LocSet(lset intersect rset)
    }

    def contains(loc: Sym): Boolean = this match {
      case Top => true
      case LocSet(set) => set.contains(loc)
    }

    def exists(f: Sym => Boolean): Boolean = this match {
      case Top => true
      case LocSet(set) => set.exists(f)
    }

    def filter(f: Sym => Boolean): AbsSym = this match {
      case Top => LocSet(symSet.filter(f))
      case LocSet(set) => LocSet(set.filter(f))
    }

    def foreach(f: Sym => Unit): Unit = this match {
      case Top => symSet.foreach(f)
      case LocSet(set) => set.foreach(f)
    }

    def foldLeft[T](initial: T)(f: (T, Sym) => T): T = this match {
      case Top => symSet.foldLeft(initial)(f)
      case LocSet(set) => set.foldLeft(initial)(f)
    }

    def map[T](f: Sym => T): Set[T] = this match {
      case Top => symSet.map(f)
      case LocSet(set) => set.map(f)
    }

    def isConcrete: Boolean = this match {
      case Top => false
      case LocSet(set) => set.size == 1
    }

    def +(loc: Sym): AbsSym = this match {
      case Top => Top
      case LocSet(set) => LocSet(set + loc)
    }

    def -(loc: Sym): AbsSym = this match {
      case Top => LocSet(symSet - loc)
      case LocSet(set) => LocSet(set - loc)
    }

    def subsLoc(locR: Sym, locO: Sym): AbsSym = this match {
      case Top => LocSet(symSet - locR + locO)
      case LocSet(set) =>
        if (set contains locR) LocSet(set - locR + locO)
        else this
    }

    def weakSubsLoc(locR: Sym, locO: Sym): AbsSym = this match {
      case Top => Top
      case LocSet(set) => LocSet(set + locO)
    }
  }
}
