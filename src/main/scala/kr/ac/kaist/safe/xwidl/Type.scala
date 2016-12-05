package kr.ac.kaist.safe.xwidl

sealed trait Type

sealed trait PrimType extends Type

case object TyNum extends PrimType

