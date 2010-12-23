package code {
package snippet {


import _root_.scala.xml.{NodeSeq, Text}

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



  info(showingVersion)
  def renderAgentResult( xhtml: NodeSeq ) = {
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


/**
  * This is first horrible and 2nd, it does not do what I wanted.
  */

    val  x= testResultNoSSL.zip(testResultSSL).map(
            row => ( row._1._1, (row._1._2._1, row._1._2._2, row._2._2._2 ) )
          ).toMap.zip(testResultNoSSLQuan).toList.map(
            row => ( row._1._1, (row._1._2._1, row._1._2._2, row._1._2._3, row._2._2._2 ) )
          ).toMap.zip(testResultSSLQuan).toList.map(
            row => ( row._1._1, (row._1._2._1, row._1._2._2, row._1._2._3, row._1._2._4, row._2._2._2 ) )
          ).toMap.zip(testResult200Scalability).toList.map(
            row => ( row._1._1, row._1._2._1, row._1._2._2, row._1._2._3, row._1._2._4, row._1._2._5, row._2._2._2 )
          )


    info(x)

    val  testResultList= testResultNoSSL.zip(
                            testResultSSL).zip(
                              testResultNoSSLQuan).zip(
                                testResultSSLQuan).zip(
                                  testResult200Scalability).map(
        row => (
                row._1._1._1._1._1,
                row._1._1._1._1._2._1,
                row._1._1._1._1._2._2,
                row._1._1._1._2._2._2,
                row._1._1._2._2._2,
                row._1._2._2._2,
                row._2._2._2
                )
      ).toList

/**
  * End of horrible code
  */


    bind("test",xhtml,
        "version" -> showingVersion,

/**
  * the conditions are supposed to look at the real data and if FAIL, then set the
  * class name to error so that it will 
  * show as red
  */

        FuncAttrBindParam(
            "classname_no_ssl", {
              ns : NodeSeq => Text(if (true) "error" else "success" )
            }, "class"
          ),

        FuncAttrBindParam(
            "classname_ssl", {
              ns : NodeSeq => Text(if ("sSL" == "FAIL") "error" else "success" )
            }, "class"
          ),

        FuncAttrBindParam(
            "classname_agent_200_scalability", {
              ns : NodeSeq => Text(if ("agent200Scalability" == "FAIL") "error" else "success" )
            }, "class"
          ),

        FuncAttrBindParam(
            "classname_no_ssl_quan", {
              ns : NodeSeq => Text(if ("noSSLQuan" == "FAIL") "error" else "success" )
            }, "class"
          ),

        FuncAttrBindParam(
            "classname_ssl_quan", {
              ns : NodeSeq => Text(if ("sSLQuan" == "FAIL") "error" else "success" )
            }, "class"
          ),

        /**
          * On the final version, these lines should set the cels to 
          * PASS or FAIL (and N/A if we don't have the results yet
          */

        "platform" -> "platform",
        "no_ssl" -> "noSSL",
        "ssl" -> "sSL",
        "agent_proxy_no_ssl_quan" -> "noSSLQuan",
        "agent_proxy_ssl_quan" -> "sSLQuan",
        "200_agent_scalability" -> "agent200Scalability"
    )

  }
  



}

}
}
