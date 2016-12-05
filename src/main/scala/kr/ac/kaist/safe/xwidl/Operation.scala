package kr.ac.kaist.safe.xwidl

class Operation (val name: String,
                 val args: List[Argument],
                 val retTy: Type,
                 val ensures: Expr

) {

}

class Argument (val name: String,
                val ty: Type
) {

}