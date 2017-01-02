package kr.ac.kaist.safe.xwidl.solver

import java.io._
import kr.ac.kaist.safe.xwidl.pprint._
import kr.ac.kaist.safe.xwidl.spec._
import scala.sys.process._

class Solver(
    interfaces: List[Interface]
) {

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
}

sealed abstract class Result

case object Verified extends Result
case class Failed(s: String) extends Result
