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

package kr.ac.kaist.safe.analyzer.models.builtin

import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils.{ AbsValue, _ }
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.util.SystemAddr
import kr.ac.kaist.safe.xwidl.spec._

import scala.collection.immutable.HashMap

object BuiltinMath extends Interface(
  name = "Math",
  kind = ECMAScriptInterface,
  constants = HashMap(
    // TODO: default to be F F F, but should be customizable
    "E" -> Num(2.7182818284590452354),
    "LN10" -> Num(2.302585092994046),
    "LN2" -> Num(0.6931471805599453),
    "LOG2E" -> Num(1.4426950408889634),
    "LOG10E" -> Num(0.4342944819032518),
    "PI" -> Num(3.1415926535897932),
    "SQRT1_2" -> Num(0.7071067811865476),
    "SQRT2" -> Num(1.4142135623730951)
  ),
  instanceAddr = SystemAddr("Math<instance>"),
  // FIXME: default to T, F, T
  operations = HashMap(
    "abs" -> Operation(
      name = "abs",
      args = List(Argument("x", TyNum)),
      retTy = TyNum,
      objAddr = SystemAddr("Math.prototype.abs<object>"),
      requires = LitExpr(LitBool(true)),
      ensures =
        IfThenElseExpr(
          BiOpExpr(VarExpr("x"), LessEq, LitExpr(LitNum(0))),
          BiOpExpr(VarExpr("ret"), EqOp, BiOpExpr(LitExpr(LitNum(0)), Minus, VarExpr("x"))),
          BiOpExpr(VarExpr("ret"), EqOp, VarExpr("x"))
        ),
      absSemOpt = Some({
        case (_, args) => AbsValue(TypeConversionHelper.ToNumber(args.head).abs)
      })
    )
  //
  //    NormalProp("acos", FuncModel(
  //      name = "Math.acos",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).acos
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("asin", FuncModel(
  //      name = "Math.asin",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).asin
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("atan", FuncModel(
  //      name = "Math.atan",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).atan
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("atan2", FuncModel(
  //      name = "Math.atan2",
  //      code = PureCode(argLen = 2, (args, st) => {
  //        val h = st.heap
  //        val resVy = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val resVx = Helper.propLoad(args, Set(AbsString("1")), h)
  //        val num = TypeConversionHelper.ToNumber(resVy).atan2(TypeConversionHelper.ToNumber(resVx))
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("ceil", FuncModel(
  //      name = "Math.ceil",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).ceil
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("cos", FuncModel(
  //      name = "Math.cos",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).cos
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("exp", FuncModel(
  //      name = "Math.exp",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).exp
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("floor", FuncModel(
  //      name = "Math.floor",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).floor
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("log", FuncModel(
  //      name = "Math.log",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).log
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("max", FuncModel(
  //      name = "Math.max",
  //      code = PureCode(argLen = 2, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("length")), h)
  //        def uintCheck(num: Double): Boolean = {
  //          val uint = num.toLong
  //          (num == uint) && (uint > 0 || (num compare 0.0) == 0)
  //        }
  //        def nArg(i: Int): AbsNumber = {
  //          TypeConversionHelper.ToNumber(
  //            Helper.propLoad(args, Set(AbsString(i.toString)), h)
  //          )
  //        }
  //        resV.pvalue.numval.getSingle match {
  //          case ConZero() => AbsNumber.Bot
  //          case ConOne(Num(0)) => AbsNumber.NegInf
  //          case ConOne(Num(num)) if uintCheck(num) => {
  //            val len = num.toInt
  //            val nanN =
  //              if ((0 until len).exists(AbsNumber.NaN <= nArg(_))) AbsNumber.NaN
  //              else AbsNumber.Bot
  //            val maxN = (1 until len).foldLeft(nArg(0)) {
  //              case (absN, i) => {
  //                val curN = nArg(i)
  //                (absN < curN).map[AbsNumber](
  //                  thenV = curN,
  //                  elseV = absN
  //                )(AbsNumber)
  //              }
  //            }
  //            nanN + maxN
  //          }
  //          case ConMany() => AbsNumber.Top
  //        }
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("min", FuncModel(
  //      name = "Math.min",
  //      code = PureCode(argLen = 2, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("length")), h)
  //        def uintCheck(num: Double): Boolean = {
  //          val uint = num.toLong
  //          (num == uint) && (uint > 0 || (num compare 0.0) == 0)
  //        }
  //        def nArg(i: Int): AbsNumber = {
  //          TypeConversionHelper.ToNumber(
  //            Helper.propLoad(args, Set(AbsString(i.toString)), h)
  //          )
  //        }
  //        resV.pvalue.numval.getSingle match {
  //          case ConZero() => AbsNumber.Bot
  //          case ConOne(Num(0)) => AbsNumber.PosInf
  //          case ConOne(Num(num)) if uintCheck(num) => {
  //            val len = num.toInt
  //            val nanN =
  //              if ((0 until len).exists(AbsNumber.NaN <= nArg(_))) AbsNumber.NaN
  //              else AbsNumber.Bot
  //            val minN = (1 until len).foldLeft(nArg(0)) {
  //              case (absN, i) => {
  //                val curN = nArg(i)
  //                (absN < curN).map[AbsNumber](
  //                  thenV = absN,
  //                  elseV = curN
  //                )(AbsNumber)
  //              }
  //            }
  //            nanN + minN
  //          }
  //          case ConMany() => AbsNumber.Top
  //        }
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("pow", FuncModel(
  //      name = "Math.pow",
  //      code = PureCode(argLen = 2, (args, st) => {
  //        val h = st.heap
  //        val resVx = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val resVy = Helper.propLoad(args, Set(AbsString("1")), h)
  //        val num = TypeConversionHelper.ToNumber(resVx).pow(TypeConversionHelper.ToNumber(resVy))
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("random", FuncModel(
  //      name = "Math.random",
  //      code = PureCode(argLen = 0, (args, st) => {
  //        val h = st.heap
  //        AbsValue(AbsNumber.Top)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("round", FuncModel(
  //      name = "Math.round",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).round
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("sin", FuncModel(
  //      name = "Math.sin",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).sin
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("sqrt", FuncModel(
  //      name = "Math.sqrt",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).sqrt
  //        AbsValue(num)
  //      })
  //    ), T, F, T),
  //
  //    NormalProp("tan", FuncModel(
  //      name = "Math.tan",
  //      code = PureCode(argLen = 1, (args, st) => {
  //        val h = st.heap
  //        val resV = Helper.propLoad(args, Set(AbsString("0")), h)
  //        val num = TypeConversionHelper.ToNumber(resV).tan
  //        AbsValue(num)
  //      })
  //    ), T, F, T)
  )
)
