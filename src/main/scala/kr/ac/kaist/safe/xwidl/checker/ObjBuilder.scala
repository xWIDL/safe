package kr.ac.kaist.safe.xwidl.checker

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.xwidl.spec._
import kr.ac.kaist.safe.analyzer._

import scala.collection.immutable.HashMap

object ObjBuilder {
  def buildObj(interface: Interface): ObjModel = {
    ObjModel(
      interface.name,
      interface.constants.map({ case (name, pVal) => NormalProp(name, PrimModel(DefaultPValue(pVal)), F, F, F) }).toList ++
        interface.operations.map({
          case (name, op) => NormalProp(name, FuncModel(
            name = interface.name + '.' + name,
            code = PureCode(argLen = op.args.length, (args, st) => {
              // A rough check: If all arguments type-match, then return a most general
              // representation of the returned type
              val h = st.heap
              val argsMatch: Boolean = op.args.view.zipWithIndex.forall({
                case (arg, i) => Helper.propLoad(args, Set(AbsString(i.toString)), h) <= arg.ty.absVal
              })
              val absArgs: List[AbsValue] = List.range(0, op.args.length).map(i => Helper.propLoad(args, Set(AbsString(i.toString)), h))
              if (argsMatch) {
                op.absSemOpt match {
                  case Some(sem) => sem(st, absArgs)
                  case None => op.retTy.absVal
                }
              } else {
                DefaultUndef.Top
              }
            })
          ), T, F, T)
        }).toList
    )
  }

  def mathInterface: Interface = Interface(
    "ECMAScriptMath", ECMAScriptInterface,
    HashMap("E" -> Num(2.7182818284590452354)),
    HashMap("abs" -> Operation("abs", List(Argument("x", TyNum)), TyNum, LitExpr(PrimBool(true)), Some({
      case (_, args) => AbsValue(TypeConversionHelper.ToNumber(args.head).abs)
    })))
  )
}

// Some examples...

