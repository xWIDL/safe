package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.util.SystemAddr
import scala.collection.immutable.HashMap

object BuiltinFoo extends Interface(
  name = "Foo",
  kind = WebAPIInterface,
  instanceAddr = SystemAddr("Foo<instance>"),
  attrs = HashMap("count" -> TyNum),
  operations = HashMap("bump" -> Operation(
    name = "bump",
    retTy = TyVoid,
    objAddr = SystemAddr("Foo.prototype.bump<object>"),
    ensures =
      BiOpExpr(
        AccessExpr(VarExpr("this"), "count"), EqOp,
        BiOpExpr(
          AccessExpr(VarExpr("old_this"), "count"),
          Plus,
          LitExpr(LitNum(1))
        )
      )
  ))
)