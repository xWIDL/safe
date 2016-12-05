package kr.ac.kaist.safe.xwidl.spec

class Operation(
    name: String,
    args: List[Argument],
    retTy: Type,
    ensures: Expr
) {

}

class Argument(
    name: String,
    ty: Type
) {

}