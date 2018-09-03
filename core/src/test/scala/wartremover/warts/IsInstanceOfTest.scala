package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.IsInstanceOf

class IsInstanceOfTest extends ResultAssertions {
  @Test def `isInstanceOf is disabled` = {
    val result = WartTestTraverser(IsInstanceOf) {
      "abc".isInstanceOf[String]
    }
    assertError(result)("isInstanceOf is disabled")
  }
  @Test def `isInstanceOf wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(IsInstanceOf) {
      @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
      val foo = "abc".isInstanceOf[String]
    }
    assertEmpty(result)
  }
  @Test def `isInstanceOf should not check macro expansions` = {
    val result = WartTestTraverser(IsInstanceOf) {
      IsInstanceOfTestMacros.is[Object, String]
    }
    assertEmpty(result)
  }
}
