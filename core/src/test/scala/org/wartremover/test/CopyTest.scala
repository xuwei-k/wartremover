package org.wartremover
package test

import org.wartremover.warts.Copy
import org.scalatest.funsuite.AnyFunSuite

class CopyTest extends AnyFunSuite with ResultAssertions {
  test("can't use copy`") {
    val x = (1, 2)
    val result = WartTestTraverser(Copy) {
      x.copy(_1 = "a")
    }
    assertError(result)("copy is disabled")
  }
}
