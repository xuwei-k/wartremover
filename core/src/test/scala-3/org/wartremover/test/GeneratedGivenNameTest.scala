package org.wartremover
package test

import scala.quoted.Expr
import scala.quoted.Quotes
import org.wartremover.warts.GeneratedGivenName
import org.scalatest.funsuite.AnyFunSuite

object GeneratedGivenNameTest {
  private given Int = 2
  private given given_hoho(using Int): String = "a"
}

class GeneratedGivenNameTest extends AnyFunSuite with ResultAssertions {
  test("report error val") {
    val result = WartTestTraverser(GeneratedGivenName) {
      GeneratedGivenNameTest.given_Int
    }
    assertError(result)("Don't use generated given name")
  }
  test("report error def") {
    val result = WartTestTraverser(GeneratedGivenName) {
      given Int = 9
      GeneratedGivenNameTest.given_hoho
    }
    assertError(result)("Don't use generated given name")
  }


  test("report error import") {
    val result = WartTestTraverser(GeneratedGivenName) {
      import GeneratedGivenNameTest.given_Int
    }
    assertError(result)("Don't use generated given name")
  }

  test("aaa") {
    val result = WartTestTraverser(GeneratedGivenName) {
      import GeneratedGivenNameTest.given Int
      def x = implicitly[Int]
    }
    assertEmpty(result)
  }

}
