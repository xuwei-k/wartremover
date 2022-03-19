package org.wartremover
package test

import org.wartremover.warts.IsInstanceOf
import org.scalatest.funsuite.AnyFunSuite

class IsInstanceOfTest2 extends AnyFunSuite with ResultAssertions {
  test("isInstanceOf should not check macro expansions") {
    val result = WartTestTraverser(IsInstanceOf) {
      IsInstanceOfTestMacros.is[Object, String]
    }
    assertEmpty(result)
  }
}
