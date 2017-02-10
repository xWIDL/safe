package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.Num
import kr.ac.kaist.safe.util.SystemAddr
import scala.collection.immutable.HashMap

/*

interface Foo {
  int count = 0;
  int count2 = 0;

  void add(int x)
       ensures this.count == old(this).count + x

  int one()
       ensures ret == 1

  void onlyFive(int x)
       requires x == 5

  void makeBiggerFive()
       ensures this.count >= 5

  void needBiggerFour()
       requires this.count >= 4

  void makeRel()
       ensures this.count >= this.count2

  void checkRel()
       requires this.count >= this.count2
}
 */

object BuiltinFoo extends Interface(
  name = "Foo",
  kind = WebAPIInterface,
  instanceAddr = SystemAddr("Foo<instance>"),
  attrs =
    HashMap(
      "count" -> (TyInt, Num(0)),
      "count2" -> (TyInt, Num(0))
    ),
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
      ),
      "makeBiggerFive" -> Operation(
        name = "makeBiggerFive",
        retTy = TyVoid,
        objAddr = SystemAddr("Foo.prototype.makeBiggerFive<object>"),
        ensures = BiOpExpr(VarExpr("this.count"), GreaterEq, LitExpr(LitInt(5)))
      ),
      "makeRel" -> Operation(
        name = "makeRel",
        retTy = TyVoid,
        objAddr = SystemAddr("Foo.prototype.makeRel<object>"),
        ensures = BiOpExpr(VarExpr("this.count"), GreaterEq, VarExpr("this.count2"))
      ),
      "checkRel" -> Operation(
        name = "checkRel",
        retTy = TyVoid,
        objAddr = SystemAddr("Foo.prototype.checkRel<object>"),
        requires = BiOpExpr(VarExpr("this.count"), GreaterEq, VarExpr("this.count2"))
      )
    )
)
