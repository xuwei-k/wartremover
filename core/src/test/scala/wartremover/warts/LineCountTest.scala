package org.wartremover
package test

import org.scalatest.FunSuite

import org.wartremover.warts.LineCount

class LineCountTest extends FunSuite with ResultAssertions {
  test("エラーにならない") {
    val result = WartTestTraverser(LineCount) {
      def shortMethod = {
        3
      }
    }
    assertEmpty(result)
  }
  test("エラーになる") {
    val result = WartTestTraverser(LineCount) {
      def tooLongMethod = List(
        1,
        2,
        3,
        4,
        5,
        6,
        7,
        8
      )
    }
    assertError(result)("tooLongMethodというメソッドが10行あります。5行以内にしてください。")
  }
}
