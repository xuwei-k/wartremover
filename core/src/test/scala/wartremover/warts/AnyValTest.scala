package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.AnyVal

class AnyValTest extends ResultAssertions {
  @Test def `AnyVal can't be inferred` = {
    val result = WartTestTraverser(AnyVal) {
      List(1, true)
    }
    assertError(result)("Inferred type containing AnyVal")
  }

  @Test def `AnyVal wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(AnyVal) {
      @SuppressWarnings(Array("org.wartremover.warts.AnyVal"))
      val x = List(1, true)
    }
    assertEmpty(result)
  }
}
