package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.Num
import kr.ac.kaist.safe.util.SystemAddr
import scala.collection.immutable.HashMap

/*

interface Foo {
  int count = 0;

  void add(int x)
       ensures this.count == old(this).count + x

  int one()
       ensures ret == 1

  void onlyFive(int x)
       requires x == 5
}
 */

object BuiltinFoo extends Interface(
  name = "Foo",
  kind = WebAPIInterface,
  instanceAddr = SystemAddr("Foo<instance>"),
  attrs = HashMap("count" -> (TyInt, Num(0))),
  operations =
    HashMap(
      "add" -> Operation(
        name = "add",
        args = List(Argument("x", TyInt)),
        retTy = TyVoid,
        objAddr = SystemAddr("Foo.prototype.add<object>"),
        ensures =
          BiOpExpr(
            VarExpr("this.count"), EqOp,
            BiOpExpr(
              VarExpr("old_this.count"),
              Plus,
              VarExpr("x")
            )
          )
      ),
      "one" -> Operation(
        name = "one",
        retTy = TyInt,
        objAddr = SystemAddr("Foo.prototype.one<object>"),
        ensures = BiOpExpr(VarExpr("ret"), EqOp, LitExpr(LitInt(1)))
      ),
      "onlyFive" -> Operation(
        name = "onlyFive",
        retTy = TyVoid,
        args = List(Argument("x", TyInt)),
        objAddr = SystemAddr("Foo.prototype.onlyFive<object>"),
        requires = BiOpExpr(VarExpr("x"), EqOp, LitExpr(LitInt(5)))
      )
    //      "biggerFive" -> Operation(
    //        name = "biggerFive",
    //        retTy = TyVoid,
    //        objAddr = SystemAddr("Foo.prototype.biggerFive<object>"),
    //        ensures = BiOpExpr(VarExpr("this.count"), GreaterEq, LitExpr(LitInt(5)))
    //      ),
    //      "biggerFour" -> Operation(
    //        name = "biggerFour",
    //        retTy = TyVoid,
    //        objAddr = SystemAddr("Foo.prototype.biggerFour<object>"),
    //        requires = BiOpExpr(VarExpr("this.count"), GreaterEq, LitExpr(LitInt(4)))
    //      )
    )
)
