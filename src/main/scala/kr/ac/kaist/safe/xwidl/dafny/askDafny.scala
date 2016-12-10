package kr.ac.kaist.safe.xwidl.dafny

import java.io._

import scala.sys.process._

object askDafny {
  def ask(query: String, binPath: String): Result = {
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

