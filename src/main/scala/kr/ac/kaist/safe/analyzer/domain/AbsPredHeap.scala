package kr.ac.kaist.safe.analyzer.domain

import scala.collection.immutable.HashMap
import kr.ac.kaist.safe.nodes.cfg.BlockId
import kr.ac.kaist.safe.xwidl.spec.{ Expr, ExprUtil }

////////////////////////////////////////////////////////////////////////////////
// concrete predicate heap type
////////////////////////////////////////////////////////////////////////////////
case class PredHeap(map: Map[BlockId, Expr])

////////////////////////////////////////////////////////////////////////////////
// predicate heap abstract domain
////////////////////////////////////////////////////////////////////////////////
trait AbsPredHeap extends AbsDomain[PredHeap, AbsPredHeap] {
  // lookup
  def get(bid: BlockId): Expr

  // remove expression
  def remove(bid: BlockId): AbsPredHeap

  def append(bid: BlockId, e: Expr): AbsPredHeap

  // check if some block ID is mapped
  def domIn(bid: BlockId): Boolean
}

trait AbsPredHeapUtil extends AbsDomainUtil[PredHeap, AbsPredHeap] {
  def apply(map: Map[BlockId, Expr]): AbsPredHeap
}

////////////////////////////////////////////////////////////////////////////////
// default predicate heap abstract domain
////////////////////////////////////////////////////////////////////////////////

object DefaultPredHeap extends AbsPredHeapUtil {
  case object Top extends Dom
  case class PredHeapMap(
    map: Map[BlockId, Expr] = HashMap()
  ) extends Dom
  lazy val Bot: AbsPredHeap = PredHeapMap()

  def alpha(heap: PredHeap): AbsPredHeap = PredHeapMap(heap.map)
  // XXX: This is just the identity function

  def apply(map: Map[BlockId, Expr]): AbsPredHeap = PredHeapMap(map)

  sealed abstract class Dom extends AbsPredHeap {
    def gamma: ConSet[PredHeap] =
      this.getMap match {
        case Some(m) => ConFin(PredHeap(m))
        case None => ConFin()
      }

    def getSingle: ConSingle[PredHeap] =
      this.getMap match {
        case Some(m) => ConOne(PredHeap(m))
        case None => ConZero()
      }

    def isBottom: Boolean = this == Bot
    def isTop: Boolean = this == Top

    def <=(that: AbsPredHeap): Boolean = (this, check(that)) match {
      case (_, Top) => true
      case (Top, _) => false
      case (left: PredHeapMap, right: PredHeapMap) =>
        if (left.map eq right.map) true
        else if (left.map.size > right.map.size) false
        else if (left.map.isEmpty) true
        else if (right.map.isEmpty) false
        else if (!(left.map.keySet subsetOf right.map.keySet)) false
        else right.map.forall((kv) => {
          val (bid, e1) = kv
          left.map.get(bid) match {
            case Some(e2) => true // FIXME: Check implication e1 -> e2
            case None => false
          }
        })
    }

    def +(that: AbsPredHeap): AbsPredHeap = (this, check(that)) match {
      case (Top, _) | (_, Top) => Top
      case (left: PredHeapMap, right: PredHeapMap) =>
        val newMap =
          if (left.map eq right.map) left.map
          else if (left.isBottom) right.map
          else if (right.isBottom) left.map
          else {
            val joinKeySet = left.map.keySet ++ right.map.keySet
            joinKeySet.foldLeft(HashMap[BlockId, Expr]())((m, key) => {
              val joinExpr = (left.map.get(key), right.map.get(key)) match {
                case (Some(e1), Some(e2)) => Some(e1 <||> e2) // TODO: rewrite?
                case (Some(e1), None) => Some(e1)
                case (None, Some(e2)) => Some(e2)
                case (None, None) => None
              }
              joinExpr match {
                case Some(e) => m.updated(key, e)
                case None => m
              }
            })
          }
        PredHeapMap(newMap)
    }

    def <>(that: AbsPredHeap): AbsPredHeap = (this, check(that)) match {
      case (left, Top) => left
      case (Top, right) => right
      case (left: PredHeapMap, right: PredHeapMap) =>
        val newMap: Map[BlockId, Expr] =
          if (left.map eq right.map) left.map
          else if (left.map.isEmpty) HashMap()
          else if (right.map.isEmpty) HashMap()
          else {
            right.map.foldLeft(left.map)(
              (m, kv) => kv match {
                case (k, v) => m.get(k) match {
                  case None => m - k
                  case Some(vv) => m + (k -> (v <&&> vv)) // TODO: rewrite?
                }
              }
            )
          }
        PredHeapMap(newMap)
    }

    override def toString: String = this match {
      case Top => "Top"
      case heap @ PredHeapMap(map) => map.toString
    }

    def get(bid: BlockId): Expr = this match {
      case Top => ExprUtil.Top
      case PredHeapMap(map) => map.get(bid) match {
        case Some(obj) => obj
        case None => ExprUtil.Bot
      }
    }

    def append(bid: BlockId, e: Expr): AbsPredHeap = this match {
      case Top => Top
      case heap @ PredHeapMap(map) => {
        map.get(bid) match {
          case Some(e0) => PredHeapMap(map.updated(bid, e0 <&&> e))
          case None => PredHeapMap(map.updated(bid, e))
        }
      }
    }

    def remove(bid: BlockId): AbsPredHeap = this match {
      case Top => Top
      case PredHeapMap(map) => PredHeapMap(map - bid)
    }

    def domIn(bid: BlockId): Boolean = this match {
      case Top => true
      case PredHeapMap(map) => map.contains(bid)
    }

    def getMap: Option[Map[BlockId, Expr]] = this match {
      case Top => None
      case PredHeapMap(map) => Some(map)
    }
  }
}
