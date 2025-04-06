package org.wartremover
package test

import org.wartremover.warts.DoobieLogHandler
import org.scalatest.funsuite.AnyFunSuite
import doobie.util.fragment.Fragment
import doobie.util.log.LogHandler

class DoobieLogHandlerTest extends AnyFunSuite with ResultAssertions {
  private val frag1: Fragment = Fragment.empty
  private def frag2: Fragment = Fragment.empty

  test("report error") {
    Seq(
      WartTestTraverser(DoobieLogHandler) {
        frag1.update
      },
      WartTestTraverser(DoobieLogHandler) {
        frag1.query[Int]
      },
      WartTestTraverser(DoobieLogHandler) {
        frag2.update
      },
      WartTestTraverser(DoobieLogHandler) {
        frag2.query[String]
      }
    ).foreach { result =>
      assertError(result)(DoobieLogHandler.message)
    }
  }

  test("don't report error if there is implicit LogHandler") {
    implicit def myLogHandler: LogHandler = ???
    val result = WartTestTraverser(DoobieLogHandler) {
      Seq(
        frag1.update,
        frag1.query[Int],
        frag2.update,
        frag2.query[Int]
      )
    }
    assertEmpty(result)
  }
}
