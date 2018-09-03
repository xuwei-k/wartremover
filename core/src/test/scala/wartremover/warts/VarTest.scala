package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Var

class VarTest extends ResultAssertions {
  @Test def `can't use var` = {
    val result = WartTestTraverser(Var) {
      var x = 10
      x
    }
    assertError(result)("var is disabled")
  }
  @Test def `Var wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Var) {
      @SuppressWarnings(Array("org.wartremover.warts.Var"))
      var x = 10
      x
    }
    assertEmpty(result)
  }
}
