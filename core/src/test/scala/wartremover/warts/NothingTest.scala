package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Nothing

class NothingTest extends ResultAssertions {
  @Test def `Nothing can't be inferred` = {
    val result = WartTestTraverser(Nothing) {
      val x = ???
      x
    }
    assertError(result)("Inferred type containing Nothing")
  }
  @Test def `Nothing wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Nothing) {
      @SuppressWarnings(Array("org.wartremover.warts.Nothing"))
      val x = ???
      x
    }
    assertEmpty(result)
  }
}
