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
      )
    )
)
