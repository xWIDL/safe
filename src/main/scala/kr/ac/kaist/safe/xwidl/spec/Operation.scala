package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.{ AbsObject, AbsState, AbsValue, DefaultBool, DefaultNull }
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.util.Address
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
  private def unify(e: Expr, s: Map[String, AbsValue]): (Expr, Map[String, AbsValue]) = {
    e match {
      case IfThenElseExpr(cond, thenBranch, elseBranch) => {
        val (cond2, s2) = unify(cond, s)
        val (then2, s3) = unify(thenBranch, s2)
        val (else2, s4) = unify(elseBranch, s3)
        (IfThenElseExpr(cond2, then2, else2), s4)
      }
      case BiOpExpr(VarExpr(v), EqOp, AbsValExpr(absVal)) => (LitExpr(LitBool(true)), s + (v -> absVal))
      case BiOpExpr(AbsValExpr(absVal), EqOp, VarExpr(v)) => (LitExpr(LitBool(true)), s + (v -> absVal))
      case BiOpExpr(le, op, re) => {
        val (le2, s2) = unify(le, s)
        val (re2, s3) = unify(re, s2)
        (BiOpExpr(le2, op, re2), s3)
      }
      case ExistsExpr(x, ty, e) => {
        val (e2, s2) = unify(e, s)
        s.get(x) match {
          case Some(absVal) => (e2.subst(x, AbsValExpr(absVal)), s - x) /* old binding? */
          case None => (ExistsExpr(x, ty, e2), s2)
        }
      }
      case _ => (e, s)
    }
  }

  def call(dafny: Solver, st: AbsState, selfObj: AbsObject,
    selfIface: Interface, argVals: List[AbsValue]): (AbsValue, AbsObject) = {

    val boundAbsVals: List[Option[(AbsValue, String)]] = requires.freeVars.map({
      case s if s.startsWith("this.") => {
        val attr = s.stripPrefix("this.")
        Some(selfObj.Get(attr, st.heap), s)
      }
      case _ => None
    }).toList

    if (!boundAbsVals.forall(_.isDefined)) {
      println("Undefined free vars")
      return (DefaultNull.Top, selfObj)
    }

    val boundAbsVals2: List[(AbsValue, String)] = boundAbsVals.flatten ++ (argVals zip args.map(_.name))

    val requiresClosed = boundAbsVals2.foldLeft(requires)({
      case (e, (absVal, y)) => e.subst(y, AbsValExpr(absVal))
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

        val oldBoundAbsVals: List[Option[(AbsValue, String)]] = requires.freeVars.map({
          case s if s.startsWith("old_this.") => {
            val attr = s.stripPrefix("old_this.")
            Some(selfObj.Get(attr, st.heap), s)
          }
          case _ => None
        }).toList


        if (!oldBoundAbsVals.forall(_.isDefined)) {
          println("Undefined free vars")
          return (DefaultNull.Top, selfObj)
        }

        val oldBoundAbsVals2 = oldBoundAbsVals.flatten ++ (argVals zip args.map(_.name))

        val ensuredOldClosed = oldBoundAbsVals2.foldLeft(requires)({
          case (e, (absVal, y)) => e.subst(y, AbsValExpr(absVal))
        })

        val ensuredUnified: (Expr, Map[String, Expr]) = unify(ensuredOldClosed.eval(st), Map())


      val selModeStream = genSelMode(concValLists.flatten)
        for (s <- selModeStream) {
          dafny.assert(ensures.substConcVals(s)) match {
            case Verified => {
              val updatedProps = s.filter({ case (_, name) => name.startsWith("this.") })
              val selfObj2 = updatedProps.foldLeft(selfObj)({
                case (selfObj, (propVal, propName)) => {
                  val prop = propName.stripPrefix("this.")
                  val (selfObj2, _) = selfObj.Put(AbsString(prop), propVal.alpha, true, st.heap) // TODO: exception
                  selfObj2
                }
              })
              s.find({ case (_, name) => name == "ret" }) match {
                case Some((retVal, _)) => return (retVal.alpha, selfObj2)
                case None => return (retTy.absTopVal, selfObj2)
              }
            }
            case _ => ()
          }
        }

        (DefaultNull.Top, selfObj)
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
