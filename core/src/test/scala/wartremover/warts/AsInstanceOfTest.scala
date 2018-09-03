package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.AsInstanceOf

class AsInstanceOfTest extends ResultAssertions {
  @Test def `asInstanceOf is disabled` = {
    val result = WartTestTraverser(AsInstanceOf) {
      "abc".asInstanceOf[String]
    }
    assertError(result)("asInstanceOf is disabled")
  }
  @Test def `asInstanceOf wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(AsInstanceOf) {
      @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
      val foo = "abc".asInstanceOf[String]
    }
    assertEmpty(result)
  }
}
