package kr.ac.kaist.safe.xwidl.checker

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.analyzer.models._
import kr.ac.kaist.safe.xwidl.spec._
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain.DefaultObject.ObjMap
import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinMath
import kr.ac.kaist.safe.xwidl.solver.Solver

import scala.collection.immutable.HashSet

object ModelBuilder {
  var solver = new Solver(List(BuiltinMath))

  private def loadArg(s: String, args: AbsValue, h: AbsHeap): AbsValue =
    Helper.propLoad(args, Set(AbsString(Str(s))), h)
  private def loadArg(i: Int, args: AbsValue, h: AbsHeap): AbsValue =
    Helper.propLoad(args, Set(AbsString(Str(i.toString))), h)

  def buildPrototype(interface: Interface): ObjModel = {
    val opHashSet = interface.operations.values.map(_.objAddr).toSet

    ObjModel(
      name = s"${interface.name}.prototype",
      props =
      // attributes
      interface.attrs.map({ case (name, (ty, pval)) => NormalProp(name, PrimModel(AbsPValue(pval)), T, F, T) }).toList ++
        // operations (which can refer to "this")
        interface.operations.map({
          case (name, op) => NormalProp(name, FuncModel(
            name = interface.name + '.' + name,
            code = BasicCode(argLen = op.args.length, opHashSet, (_, _) => ???, Some((args, st, cp, preds) => { // HACK
              // A rough check: If all arguments type-match, then return a most general
              // representation of the returned type

              val thisBinding = st.context.thisBinding
              val (thisLoc, state, es) = TypeConversionHelper.ToObject(thisBinding, st, op.objAddr)
              val h = state.heap
              val (retH, retV, retPHeap, excSet) = thisLoc.foldLeft((h, AbsValue.Bot, AbsPredHeap.Bot, es)) {
                case ((h, value, pheap, excSet), loc) => {
                  val thisObj = h.get(loc)

                  val length = loadArg("length", args, h).pvalue.numval

                  val actualLen: AbsNumber = AbsNumber(op.args.length)
                  if (!(actualLen <= length)) {
                    println("[WARNING] unmatched argument number")
                    (h, DefaultNull.Top, pheap, excSet)
                  } else {
                    val argsMatch: List[(Int, Boolean)] = op.args.view.zipWithIndex.map({
                      case (arg, i) => (i, loadArg(i, args, h) <= arg.ty.absTopVal)
                    }).toList

                    val absArgs: List[AbsValue] = List.range(0, op.args.length).map(loadArg(_, args, h))

                    if (argsMatch.forall({ case (_, matched) => matched })) {

                      op.absSemOpt match {
                        case Some(sem) => (h, sem(st, absArgs), pheap, excSet)
                        case None => {

                          val (retVal, thisObj2, retPHeap) = op.call(solver, st, thisObj, interface, absArgs, cp.node)

                          (h.update(loc, thisObj2), if (retVal.pvalue != null) {
                            retVal + value
                          } else {
                            value
                          }, retPHeap + pheap, excSet) // TODO: why pvalue will be null?
                        }
                      }
                    } else {
                      // Print out what is wrong
                      argsMatch.filter({ case (i, matched) => !matched }).foreach({
                        case (i, _) => {
                          println(s"[xWIDL-TYPE-ERROR: ${preds.map(_.node.span.toString)}]" +
                            s" ${i}th argument ${absArgs(i)} of " +
                            s"${interface.name}.$name is not ${op.args(i).ty}")
                        }
                      })
                      (h, DefaultNull.Top, pheap, excSet)
                    }
                  }
                }
              }

              // Then returned undefined
              val excSt = st.raiseException(excSet)
              (AbsState(retH, state.context, retPHeap), excSt, retV)
            }))
          ), T, F, T)
        }).toList
    )
  }

  def buildProtoFunc(interface: Interface): FuncModel = {
    val protoObj = buildPrototype(interface)
    FuncModel(
      name = interface.name,
      // TODO: more general construct
      construct = Some(BasicCode(
        argLen = 0,
        addrSet = HashSet(interface.instanceAddr),
        code = (args, st) => {
          val h = st.heap

          val o: AbsObject = ObjMap(AbsMapEmpty)
          val o2 = o
            .update(IClass, InternalValueUtil(AbsString(interface.name)))
            .update(IPrototype, InternalValueUtil(protoObj.loc))
            .update(IExtensible, InternalValueUtil(AbsBool(true)))

          val retObj = interface.attrs.foldLeft(o2)({
            case (o, (name, (_, pval))) => {
              o.update(name, AbsDataProp(AbsValue(pval), AbsBool(true), AbsBool(false), AbsBool(true)))
            }
          })

          val arrAddr = interface.instanceAddr
          val state = st.oldify(arrAddr)
          val arrLoc = Loc(arrAddr, Recent)
          val retH = state.heap.update(arrLoc, retObj.oldify(arrAddr))
          val excSt = state.raiseException(ExcSetEmpty)
          (AbsState(retH, state.context), excSt, AbsLoc(arrLoc))
        }
      )),
      protoModel = Some(protoObj, F, F, F)
    )
  }

  // TODO: merge two ways of building methods
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

              op.absSemOpt match {
                case Some(sem) => sem(st, absArgs)
                case None => op.retTy.absTopVal
              }
            } else {
              // Print out what is wrong
              argsMatch.filter({ case (i, matched) => !matched }).foreach({
                case (i, _) => {
                  println(s"[xWIDL-TYPE-ERROR] ${i}th argument ${absArgs(i)} of " +
                    s"${interface.name}.$name is not ${op.args(i).ty}") // TODO: Precise call-site
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