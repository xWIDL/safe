package kr.ac.kaist.safe.xwidl.checker

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.xwidl.spec._
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinMath
import kr.ac.kaist.safe.xwidl.dafny.Dafny

object ObjBuilder {
  var dafny = new Dafny("/home/zz/xwidl/dafny/Binaries/dafny", List(BuiltinMath))

  def buildPrototype(interface: Interface): ObjModel = {
    val opHashSet = interface.operations.values.map(_.objAddr).toSet
    ObjModel(
      name = s"${interface.name}.prototype",
      props =
      // attributes
      interface.attrs.map({ case (name, ty) => NormalProp(name, PrimModel(ty.absTopPVal), T, F, T) }).toList ++
        // operations (which can refer to "this")
        interface.operations.map({
          case (name, op) => NormalProp(name, FuncModel(
            name = interface.name + '.' + name,
            code = BasicCode(argLen = op.args.length, opHashSet, (args, st) => {
              // A rough check: If all arguments type-match, then return a most general
              // representation of the returned type

              val thisBinding = st.context.thisBinding
              val (thisLoc, state, es) = TypeConversionHelper.ToObject(thisBinding, st, op.objAddr)
              val h = state.heap
              val (retH, retV, excSet) = thisLoc.foldLeft((h, AbsValue.Bot, es)) {
                case ((h, value, excSet), loc) => {
                  val thisObj = h.get(loc)

                  val argsMatch: List[(Int, Boolean)] = op.args.view.zipWithIndex.map({
                    case (arg, i) => (i, Helper.propLoad(args, Set(AbsString(i.toString)), h) <= arg.ty.absTopVal)
                  }).toList

                  val absArgs: List[AbsValue] = List.range(0, op.args.length)
                    .map(i => Helper.propLoad(args, Set(AbsString(i.toString)), h))

                  if (argsMatch.forall({ case (_, matched) => matched })) {

                    val argPreds: List[Predicate] = op.args.view.zipWithIndex.map({
                      case (arg, i) => {
                        val argVal = Helper.propLoad(args, Set(AbsString(i.toString)), h)
                        Predicate("x", arg.ty, argVal.pvalue.gamma2("x"))
                      }
                    }).toList

                    op.absSemOpt match {
                      case Some(sem) => (h, sem(st, absArgs), excSet)
                      case None => {
                        val retVal = op.call(dafny, st, thisObj, interface, absArgs) // TODO: Update this
                        (h, retVal, excSet)
                      }
                    }
                  } else {
                    // Print out what is wrong
                    argsMatch.filter({ case (i, matched) => !matched }).foreach({
                      case (i, _) => {
                        println(s"$i's argument of ${interface.name}.$name is of wrong type:" +
                          s"${absArgs(i)} is not ${op.args(i).ty}") // FIXME: Use warning mechanism
                      }
                    })
                    (h, DefaultNull.Top, excSet)
                  }
                }
              }

              // Then returned undefined
              val excSt = st.raiseException(excSet)
              (AbsState(retH, state.context), excSt, retV)
            })
          ), T, F, T)
        }).toList
    )
  }

  def buildProtoFunc(interface: Interface): FuncModel = {
    FuncModel(
      name = interface.name,
      // TODO: code
      // TODO: construct
      protoModel = Some(buildPrototype(interface), F, F, F)
    )
  }

  def buildObj(interface: Interface): ObjModel = {
    ObjModel(
      name = interface.name,
      props = interface.constants.map({
      case (name, pVal) =>
        NormalProp(name, PrimModel(DefaultPValue(pVal)), F, F, F)
    }).toList ++
      interface.operations.map({
        case (name, op) => NormalProp(name, FuncModel(
          name = interface.name + '.' + name,
          code = PureCode(argLen = op.args.length, (args, st) => {
            // A rough check: If all arguments type-match, then return a most general
            // representation of the returned type
            val h = st.heap

            val argsMatch: List[(Int, Boolean)] = op.args.view.zipWithIndex.map({
              case (arg, i) => (i, Helper.propLoad(args, Set(AbsString(i.toString)), h) <= arg.ty.absTopVal)
            }).toList

            val absArgs: List[AbsValue] = List.range(0, op.args.length)
              .map(i => Helper.propLoad(args, Set(AbsString(i.toString)), h))

            if (argsMatch.forall({ case (_, matched) => matched })) {

              val argPreds: List[Predicate] = op.args.view.zipWithIndex.map({
                case (arg, i) => {
                  val argVal = Helper.propLoad(args, Set(AbsString(i.toString)), h)
                  Predicate("x", arg.ty, argVal.pvalue.gamma2("x"))
                }
              }).toList

              // println(absArgs)

              // op.call(dafny, st,)
              // dafny.call(interface.name, name, argPreds, op.retTy)

              op.absSemOpt match {
                case Some(sem) => sem(st, absArgs)
                case None => op.retTy.absTopVal
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