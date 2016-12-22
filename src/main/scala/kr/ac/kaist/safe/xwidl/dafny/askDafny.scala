package kr.ac.kaist.safe.xwidl.dafny

import java.io._

import kr.ac.kaist.safe.analyzer.domain.{ AbsValue, DefaultNull }
import kr.ac.kaist.safe.xwidl.pprint._
import kr.ac.kaist.safe.xwidl.spec.Interface

import scala.collection.mutable
import scala.sys.process._

sealed trait Statement extends Pack
case class CallStmt(iname: String, fname: String, args: List[AbsValue]) extends Statement {
  def pack: Doc = {
    text(iname) <> text(".") <> text(fname) // FIXME: no return value
  }
}

class Dafny(
  binPath: String,
    interfaces: List[Interface]
) {
  var mainStmts: mutable.MutableList[Statement] = _
  var mainArgs: mutable.HashMap[String, String] = _ // FIXME: naive encoding of Dafny type

  def call(iname: String, fname: String, args: List[AbsValue]): Option[AbsValue] = {

    val ivar = "global_" + iname
    if (mainArgs.get(ivar).isEmpty) {
      mainArgs += (ivar -> iname)
    }

    mainStmts.+=(CallStmt(ivar, fname, args))

    ask() match {
      case Verified => Some(DefaultNull.Top)
      case Failed(output) => {
        println(output)
        None
      }
    }
  }

  def query: String = {
    val ifaces = interfaces.map(i => showDoc(100, i.pack)).fold("")((a, b) => a + b)
    val argsStr: List[Doc] = mainArgs.map({ case (k, v) => text(k + " : " + v) }).toList
    val mainDoc: Doc = text("method") <+> text("Main") <+> parens(splitBy(argsStr, text(", "))) <+> braces(mainStmts.map(_.pack).fold(text(""))((a, b) => a </> b))
    showDoc(100, mainDoc)
  }

  def ask(): Result = {
    val tempFile = File.createTempFile("xwidl", ".dfy")
    val writer = new FileWriter(tempFile)
    writer.write(query)
    writer.flush()
    writer.close()

    val output = (binPath + " /nologo /compile:0 " + tempFile) !!

    if (output.startsWith("Dafny program verifier finished with")) {
      Verified
    } else {
      Failed(output)
    }
  }
}

sealed abstract class Result

case object Verified extends Result
case class Failed(s: String) extends Result
