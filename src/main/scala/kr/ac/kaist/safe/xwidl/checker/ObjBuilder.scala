package kr.ac.kaist.safe.xwidl.checker

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.xwidl.spec._
import kr.ac.kaist.safe.analyzer._

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
              val argPreds: List[Predicate] = op.args.view.zipWithIndex.map({
                case (arg, i) => {
                  val argVal = Helper.propLoad(args, Set(AbsString(i.toString)), h)
                  arg.ty match {
                    case TyNum => Predicate("x", TyNum, argVal.pvalue.gamma2("x"))
                    // FIXME: other cases
                  }
                }
              }).toList

              val argsMatch: List[(Int, Boolean)] = op.args.view.zipWithIndex.map({
                case (arg, i) => (i, Helper.propLoad(args, Set(AbsString(i.toString)), h) <= arg.ty.absVal)
              }).toList

              val absArgs: List[AbsValue] = List.range(0, op.args.length)
                .map(i => Helper.propLoad(args, Set(AbsString(i.toString)), h))

              if (argsMatch.forall({ case (_, matched) => matched })) {
                op.absSemOpt match {
                  case Some(sem) => sem(st, absArgs)
                  case None => op.retTy.absVal
                }
              } else {
                // Print out what is wrong
                argsMatch.filter({ case (i, matched) => !matched }).foreach({
                  case (i, _) => {
                    println(s"$i's argument of ${interface.name}.$name is of wrong type:" +
                      s"${absArgs(i)} is not ${op.args(i).ty}") // FIXME: Use warning mechanism
                  }
                })

                // Then returned undefined
                DefaultUndef.Top
              }
            })
          ), T, F, T)
        }).toList
    )
  }
}