package kr.ac.kaist.safe.xwidl.spec

import kr.ac.kaist.safe.analyzer.domain.Num
import kr.ac.kaist.safe.util.SystemAddr
import scala.collection.immutable.HashMap

object BuiltinFoo extends Interface(
  name = "Foo",
  kind = WebAPIInterface,
  instanceAddr = SystemAddr("Foo<instance>"),
  attrs = HashMap("count" -> (TyNum, Num(0))),
  operations = HashMap("bump" -> Operation(
    name = "bump",
    retTy = TyVoid,
    objAddr = SystemAddr("Foo.prototype.bump<object>"),
    ensures =
      BiOpExpr(
        VarExpr("this.count"), EqOp,
        BiOpExpr(
          VarExpr("old_this.count"),
          Plus,
          LitExpr(LitNum(1))
        )
      )
  ))
)
