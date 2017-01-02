package kr.ac.kaist.safe.xwidl.dafny

import java.io._

import kr.ac.kaist.safe.analyzer.domain.{ AbsValue, DefaultNull }
import kr.ac.kaist.safe.xwidl.pprint._
import kr.ac.kaist.safe.xwidl.spec._

import scala.collection.mutable
import scala.io.Source
import scala.sys.process._

sealed trait Statement extends Pack
case class CallStmt(iname: String, fname: String,
    args: List[String], retTy: Type, retId: Int) extends Statement {
  def pack: Doc = {
    val prefix = retTy match {
      case TyVoid => ""
      case _ => "var ret_" + retId + " := "
    }

    text(prefix) <> text(iname) <> text(".") <>
      text(fname) <> parens(splitBy(args.map(text), text(", "))) <> text(";") // FIXME: no return value
  }
}

class Dafny(
    binPath: String,
    interfaces: List[Interface]
) {
  var mainStmts: mutable.MutableList[Statement] = mutable.MutableList()
  var mainRequires: mutable.MutableList[Expr] = mutable.MutableList()
  var mainArgs: mutable.HashMap[String, String] = mutable.HashMap() // FIXME: naive encoding of Dafny type
  var mainUid: Int = 0

  def call(iname: String, fname: String, args: List[ConcVal], retTy: Type): Option[AbsValue] = {

    val ivar = "global_" + iname
    if (mainArgs.get(ivar).isEmpty) {
      mainArgs += (ivar -> iname)
    }

    val argReprs: List[String] = args.map({
      case PredicateVal(binder, ty, body, _) => {
        val argvar = binder + mainUid
        mainUid += 1
        mainArgs += (argvar -> showDoc(80, ty.pack))
        mainRequires.+=(body)
        argvar
      }
      case PreciseVal(v) => v.toString
    })

    mainStmts.+=(CallStmt(ivar, fname, argReprs, retTy, mainUid))
    mainUid += 1

    ask(query) match {
      case Verified => {
        println("Verified")
        Some(DefaultNull.Top)
      }
      case Failed(output) => {
        println("Failed: " + output)
        None
      }
    }
  }

  def assert(e: Expr): Result = {

    val doc: Doc = parens(text("assert") <+> e.packZ3) </> parens(text("check-sat"))

    val queryStr = showDoc(120, doc)

    val tempFile = File.createTempFile("xwidl", ".z3")
    val writer = new FileWriter(tempFile)
    writer.write(queryStr)
    writer.flush()
    writer.close()

    val stdoutStream = new ByteArrayOutputStream
    val stderrStream = new ByteArrayOutputStream
    val stdoutWriter = new PrintWriter(stdoutStream)
    val stderrWriter = new PrintWriter(stderrStream)
    val exitCode = ("z3 " + tempFile) ! ProcessLogger(stdoutWriter.println, stderrWriter.println)
    stdoutWriter.close()
    stderrWriter.close()

    val output = stdoutStream.toString
    if (exitCode == 0) {
      if (output.startsWith("sat")) {
        Verified
      } else {
        Failed(output + queryStr)
      }
    } else {
      Failed(output + stderrStream.toString + queryStr)
    }
  }

  def query: String = {
    val ifaces: Doc = interfaces.map(_.pack).fold(nil)((a, b) => a </> b)
    val argsStr: List[Doc] = mainArgs.map({ case (k, v) => text(k + " : " + v) }).toList
    val mainDoc: Doc = text("method") <+> text("Main") <+> parens(splitBy(argsStr, text(", "))) <+>
      text("requires") <+> mainRequires.map(_.pack).fold(text("true"))({ case (a, b) => a <+> text("&&") <+> b }) </>
      braces(mainStmts.map(_.pack).fold(text(""))((a, b) => a </> b))
    showDoc(100, ifaces </> mainDoc)
  }

  def ask(queryStr: String): Result = {
    val tempFile = File.createTempFile("xwidl", ".dfy")
    val writer = new FileWriter(tempFile)
    writer.write(queryStr)
    writer.flush()
    writer.close()

    println(binPath + " /nologo /compile:0 " + tempFile)
    val stdoutStream = new ByteArrayOutputStream
    val stderrStream = new ByteArrayOutputStream
    val stdoutWriter = new PrintWriter(stdoutStream)
    val stderrWriter = new PrintWriter(stderrStream)
    val exitCode = (binPath + " /nologo /compile:0 " + tempFile) ! ProcessLogger(stdoutWriter.println, stderrWriter.println)
    stdoutWriter.close()
    stderrWriter.close()

    val output = stdoutStream.toString
    if (exitCode == 0) {
      if (output.startsWith("Dafny program verifier finished with")) {
        Verified
      } else {
        Failed(output + queryStr)
      }
    } else {
      Failed(output + stderrStream.toString + queryStr)
    }
  }
}

sealed abstract class Result

case object Verified extends Result
case class Failed(s: String) extends Result
