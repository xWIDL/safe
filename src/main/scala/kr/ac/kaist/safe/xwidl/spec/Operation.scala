package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsObject, AbsState, AbsValue, DefaultBool, DefaultNull, DefaultValue }
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.util.{ Address, NodeUtil }
import kr.ac.kaist.safe.xwidl.solver.{ Solver, Verified }

case class OperationException(s: String) extends Exception(s)

case class Operation(
    name: String,
    args: List[Argument] = List(),
    retTy: Type,
    objAddr: Address,
    requires: Expr = LitExpr(LitBool(true)),
    ensures: Expr = LitExpr(LitBool(true)),
    absSemOpt: Option[(AbsState, List[AbsValue]) => AbsValue] = None
) {

  // TODO: meet cases?
  private def genSelMode(concValLists: List[(List[ConcVal], String)]): List[List[(ConcVal, String)]] = {
    concValLists.foldRight(List(List[(ConcVal, String)]()))({
      case ((vals, name), lss) => {
        lss.flatMap(ls => vals.map(v => (v, name) :: ls))
      }
    })
  }

  // Needs soundness proof
  private def unify(e: Expr, s: Map[String, AbsValue], eq: List[(String, String)]): (Expr, Map[String, AbsValue], List[(String, String)]) = {
    e match {
      case IfThenElseExpr(cond, thenBranch, elseBranch) => {
        val (cond2, s2, eq2) = unify(cond, s, eq)
        val (then2, s3, eq3) = unify(thenBranch, s2, eq2)
        val (else2, s4, eq4) = unify(elseBranch, s3, eq3)
        (IfThenElseExpr(cond2, then2, else2), s4, eq4)
      }
      case BiOpExpr(VarExpr(v), EqOp, AbsValExpr(absVal)) =>
        (LitExpr(LitBool(true)), s + (v -> absVal), eq)
      case BiOpExpr(AbsValExpr(absVal), EqOp, VarExpr(v)) =>
        (LitExpr(LitBool(true)), s + (v -> absVal), eq)
      case BiOpExpr(VarExpr(v1), EqOp, VarExpr(v2)) =>
        (LitExpr(LitBool(true)), s, (v1, v2) :: eq)
      case BiOpExpr(le, op, re) => {
        val (le2, s2, eq2) = unify(le, s, eq)
        val (re2, s3, eq3) = unify(re, s2, eq2)
        (BiOpExpr(le2, op, re2), s3, eq3)
      }
      case ExistsExpr(x, ty, e) => {
        val (e2, s2, eq2) = unify(e, s, eq)
        s.get(x) match {
          case Some(absVal) => (e2.subst(x, AbsValExpr(absVal)), s - x, eq2) /* old binding? */
          case None => (ExistsExpr(x, ty, e2), s2, eq2)
        }
      }
      case _ => (e, s, eq)
    }
  }

  private def findEq(eq: List[(String, String)], x: String): (List[(String, String)], Set[String]) = {
    val subXs = eq.filter({ case (u, v) => u == x || v == x }).flatMap({ case (u, v) => Set(u, v) })
    val (eq2, s) = findEq(eq.filter({ case (u, v) => u != x && v != x }), x)
    (eq2, s ++ subXs)
  }

  private def eqResolve(eq: List[(String, String)], s: Map[String, AbsValue]): (List[(String, String)], Map[String, AbsValue]) = {
    s.foldLeft(eq, s)({
      case ((eq, s), (x, v)) => {
        if (eq.isEmpty) {
          (eq, s)
        } else {
          val (eq2, eqX) = findEq(eq, x)
          (eq2, s ++ eqX.map((_, v)))
        }
      }
    })
  }

  def call(dafny: Solver, st: AbsState, selfObj: AbsObject, selfIface: Interface, argVals: List[AbsValue]): (AbsValue, AbsObject) = {

    val argValsMap: Map[String, AbsValue] = args.map(_.name).zip(argVals).toMap

    val boundAbsVals: List[Option[(String, AbsValue)]] = requires.freeVars.map({
      case s if s.startsWith("this.") => {
        val attr = s.stripPrefix("this.")
        Some(s, selfObj.Get(attr, st.heap))
      }
      case s if s.contains(".") && !s.startsWith("this.") => {
        val (x, attr) = s.splitAt(s.indexOf(".")) // FIXME: multi-level access
        val attr2 = attr.tail
        argValsMap.get(x) match {
          case Some(xVal) => Some(s, xVal.locset.map(st.heap.get(_).Get(attr2, st.heap)).fold(DefaultValue.Bot)((v1, v2) => v1 + v2))
          case None => None
        }
      }
      case _ => None
    }).toList

    if (!boundAbsVals.forall(_.isDefined)) {
      println("Undefined free vars")
      return (DefaultNull.Top, selfObj)
    }

    val boundAbsVals2: List[(String, AbsValue)] = boundAbsVals.flatten ++ argValsMap

    val requiresClosed = boundAbsVals2.foldLeft(requires)({
      case (e, (y, absVal)) => e.subst(y, AbsValExpr(absVal))
    })

    // it might not be closed -- since some AbsValue are actually symbol
    // in that case, we will need to conjunct this with the constraint
    // and use SMT to solve that awkward condition

    val reqSatisfied = requiresClosed.eval(st) match {
      case Some(AbsValExpr(v)) => DefaultBool.True <= v.pvalue.boolval // abstract judgement
      case Some(e) => dafny.assert(BiOpExpr(e, And, st.constraint)) match { // symbolic judgement
        case Verified => true
        case _ => false
      }
      case None => false
    }

    if (reqSatisfied) {
      // Instead of generating the stream for everything, we should
      // conduct a structural analysis of the post-cond expression,
      // try to apply full abstraction, equitional unification, and
      // finally concreatization (and update the trace if everything is fine)
      // This is becoming more and more like a symbolic executor

      val oldBoundAbsVals: List[(String, AbsValue)] = ensures.freeVars.flatMap({
        case s if s.startsWith("old_this.") => {
          val attr = s.stripPrefix("old_this.")
          List((s, selfObj.Get(attr, st.heap)))
        }
        case s if s.contains(".") && !s.startsWith("this.") => {
          val (x, attr) = s.splitAt(s.indexOf(".")) // FIXME: multi-level access
          val attr2 = attr.tail
          argValsMap.get(x) match {
            case Some(xVal) =>
              List((s, xVal.locset.map(st.heap.get(_).Get(attr2, st.heap)).fold(DefaultValue.Bot)((v1, v2) => v1 + v2)))
            case None => {
              println("Something is wrong")
              List()
            }
          }
        }
        case s if s.startsWith("this.") => List()
        case _ => {
          println("Something is wrong")
          List()
        }
      }).toList

      val oldBoundAbsVals2 = argValsMap ++ oldBoundAbsVals

      val ensuredOldClosed = oldBoundAbsVals2.foldLeft(ensures)({
        case (e, (y, absVal)) => e.subst(y, AbsValExpr(absVal))
      })

      ensuredOldClosed.eval(st) match { // Partial evaluation
        case None => {
          println("Something goes wrong")
          (DefaultValue.Top, selfObj)
        }
        case Some(e) => {

          val (e2, s, eq) = unify(e, Map(), List())

          // TODO: dep resolution, iteration
          // val Some(e3) = e2.eval(st)

          val (eq2, s2) = eqResolve(eq, s)

          val fullyFreeVars = e2.freeVars ++ eq2.flatMap({ case (u, v) => Set(u, v) })

          if (fullyFreeVars.isEmpty) {
            // DO I NEED TO CHECK AGAIN?

            // substitute
            val retVal = s2.getOrElse("ret", DefaultValue.Top) // use default value for certain type?

            // TODO: the heap effect

            // self effect
            val selfObj2 = s2.foldLeft(selfObj)({
              case (selfObj, (x, xVal)) => {
                if (x.startsWith("this.")) {
                  val attr = x.stripPrefix("this.")
                  selfObj.update(attr, AbsDataProp(xVal))
                } else {
                  selfObj
                }
              }
            })

            (retVal, selfObj2)
          } else {
            // Use symbolic constraint...
            // DO I NEED TO CHECK AGAIN?

            // substitute -- naive assuming that ret won't be leaked
            val retVal = s2.getOrElse("ret", DefaultValue.Top) // use default value for certain type?

            // TODO: the heap effect

            // self effect
            val selfObj2 = s2.foldLeft(selfObj)({
              case (o, (x, xVal)) => {
                if (x.startsWith("this.")) {
                  val attr = x.stripPrefix("this.")
                  o.update(attr, AbsDataProp(xVal))
                } else {
                  o
                }
              }
            })

            // TODO: the heap symbolic effect

            // self symbolic effect
            val (selfObj3, e3) = fullyFreeVars.foldLeft(selfObj2, e2)({
              case ((o, e), x) => {
                if (x.startsWith("this.")) {
                  val attr = x.stripPrefix("this.")
                  val sym = NodeUtil.freshName(attr)
                  (o.update(attr, AbsDataProp(AbsValue(sym))), e.subst(x, VarExpr(sym)))
                } else {
                  (o, e)
                }
              }
            })

            // TODO: the constraint effect

            (retVal, selfObj3)
          }
        }
      }
    } else {
      println("Something is wrong with spec")
      (DefaultNull.Top, selfObj)
    }
  }
}

case class Argument(
  name: String,
  ty: Type
)

// FIXME: variadic functions
