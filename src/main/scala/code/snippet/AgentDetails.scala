package code {
package snippet {


import _root_.scala.xml.{NodeSeq, Text, Elem}

import _root_.net.liftweb._
import util._
import common.Logger
import http.SHtml._
import http.S._
import http.js.JsCmds.{SetHtml, SetValueAndFocus}
import Helpers._


class AgentDetails extends Logger {

  /**
    * Generate the Test Result view section
    */

  val showingVersion= http.S.param("v") openOr("ERROR") 



  //info(showingVersion)



  def renderAgentResult = {
    val testResultNoSSL= Map(
                            "Sol 10 Sparc 32Bit" -> ("Solaris", "PASS"), 
                            "Sol 10 Sparc 64"    -> ("Solaris", "PASS"), 
                            "Sol 10 x86 64"      -> ("Solaris", "FAIL") 
                            )
    val testResultSSL= Map(
                            "Sol 10 Sparc 32Bit" -> ("Solaris", "PASS"), 
                            "Sol 10 Sparc 64"    -> ("Solaris", "FAIL"),
                            "Sol 10 x86 64"      -> ("Solaris", "FAIL") 
                            )
    val testResultNoSSLQuan= Map(
                            "Sol 10 Sparc 32Bit" -> ("Solaris", "FAIL"), 
                            "Sol 10 Sparc 64"    -> ("Solaris", "PASS"),
                            "Sol 10 x86 64"      -> ("Solaris", "FAIL") 
                            )
    val testResultSSLQuan= Map(
                            "Sol 10 Sparc 32Bit" -> ("Solaris", "FAIL"), 
                            "Sol 10 Sparc 64"    -> ("Solaris", "FAIL"),
                            "Sol 10 x86 64"      -> ("Solaris", "FAIL") 
                            )
    val testResult200Scalability= Map(
                            "Sol 10 Sparc 32Bit" -> ("Solaris", "PASS"), 
                            "Sol 10 Sparc 64"    -> ("Solaris", "PASS"),
                            "Sol 10 x86 64"      -> ("Solaris", "PASS") 
                            )


    case class Result(OS: String, pass: Boolean) {
      def clss = if (pass) "success" else "error"
    }

    case class TestRow(tests: Map[Int, Option[Result]])


    // a list of all our tests
    val tests = List(testResultNoSSL, testResultSSL, testResultNoSSLQuan, testResultSSLQuan, testResult200Scalability)

    // a set of the keys of the tests
    val osSet = Set(tests.flatMap(_.keys) :_*)

    val results: Map[String, TestRow] = Map(osSet.toSeq.map{
      set =>
        (set, TestRow(Map(tests.zipWithIndex.map {
          case (test, idx) => idx -> test.get(set).map{
                                                      case (os, pass) => Result(os, pass == "PASS")
                                                      }
        } :_*)))
      }:_*)


    def dd(f: NodeSeq => NodeSeq): NodeSeq => NodeSeq = {
      case e: Elem => f(e.child)
      case x => f(x)
    }

    ClearClearable andThen
    "h2 *" #> dd(_ ++ Text(showingVersion)) &
    "#row *" #> results.map {
      case (name, row) => "#col" #> (
        ("* *" #> name) :: row.tests.toList.map {
          case (pos, Some(res)) => "* *" #> name   & "* [class]" #> res.clss
          case (pos, _) => "* *" #> "N/A" & "* [class]" #> "notice"
        }
        )
    }
  }
}

}
}
