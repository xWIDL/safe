/*
 credit: A Prettier Printer by Philip Wadler
 */

package kr.ac.kaist.safe.xwidl.pprint

import scala.annotation.tailrec

trait Doc {
  def <>(d: Doc): Doc = {
    new <>(this, d)
  }

  def :<|>(d: Doc): Doc = {
    new :<|>(this, d)
  }

  final def <+>(y: Doc): Doc = this <> text(" ") <> y
  final def </>(y: Doc): Doc = this <> line <> y
  final def <+/>(y: Doc): Doc = this <> (text(" ") :<|> line) <> y
}

object spread { def apply(xs: List[Doc]): Doc = xs.fold(nil)({ case (x, y) => x <+> y }) }

object stack { def apply(xs: List[Doc]): Doc = xs.fold(nil)({ case (x, y) => x </> y }) }

object bracket {
  def apply(l: String, x: Doc, r: String): Doc =
    group(text(l) <> nest(2, line <> x) <> line <> text(r))
}

case object nil extends Doc
final case class <>(d1: Doc, d2: Doc) extends Doc
case class nest(i: Int, d: Doc) extends Doc
case class text(s: String) extends Doc
case object line extends Doc
final case class :<|>(d1: Doc, d2: Doc) extends Doc

sealed trait RDoc

case object RNil extends RDoc
case class RText(s: String, d: RDoc) extends RDoc
case class RLine(i: Int, d: RDoc) extends RDoc

object group { def apply(x: Doc): Doc = flatten(x) :<|> x }

object flatten {
  def apply(d: Doc): Doc = d match {
    case `nil` => nil
    case x <> y => flatten(x) <> flatten(y)
    case nest(i, x) => nest(i, flatten(x))
    case text(s) => text(s)
    case `line` => text(" ")
    case x :<|> _ => flatten(x)
  }
}

object layout {
  def apply(doc: RDoc): String = doc match {
    case RNil => ""
    case RText(s, d) => s + layout(d)
    case RLine(i, x) => "\n" + (' '.toString * i) + layout(x)
  }
}

object best {
  def apply(w: Int, k: Int, x: Doc): RDoc = be(w, k, List((0, x)))
}

object be {
  def apply(w: Int, k: Int, xs: List[(Int, Doc)]): RDoc = xs match {
    case (_, `nil`) :: z => be(w, k, z)
    case (i, x <> y) :: z => be(w, k, (i, x) :: (i, y) :: z)
    case (i, nest(j, x)) :: z => be(w, k, (i + j, x) :: z)
    case (i, text(s)) :: z => RText(s, be(w, k + s.length, z))
    case (i, `line`) :: z => RLine(i, be(w, i, z))
    case (i, x :<|> y) :: z => better(w, k, be(w, k, (i, x) :: z), be(w, k, (i, y) :: z))
    case _ => RNil
  }
}

object better {
  def apply(w: Int, k: Int, x: RDoc, y: RDoc): RDoc =
    if (fits(w - k, x)) {
      x
    } else {
      y
    }
}

object fits {
  @tailrec
  def apply(w: Int, x: RDoc): Boolean = {
    if (w < 0) {
      false
    } else {
      x match {
        case RText(s, y) => fits(w - s.length, y)
        case _ => true
      }
    }
  }
}

object pretty {
  def apply(w: Int, x: Doc): String = layout(best(w, 0, x))
}
