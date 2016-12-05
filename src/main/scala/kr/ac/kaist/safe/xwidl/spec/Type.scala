package kr.ac.kaist.safe.xwidl.spec

sealed trait Type

sealed trait PrimType extends Type

case object TyNum extends PrimType

