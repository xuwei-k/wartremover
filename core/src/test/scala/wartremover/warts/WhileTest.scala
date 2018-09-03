package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.While

class WhileTest extends ResultAssertions {
  @Test def `while is disabled` = {
    val result = WartTestTraverser(While) {
      while (true) {
        println()
      }
    }
    assertError(result)("while is disabled")
  }

  @Test def `do while is disabled` = {
    val result = WartTestTraverser(While) {
      do {
        println()
      } while (true)
    }
    assertError(result)("while is disabled")
  }

  @Test def `while wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(While) {
      @SuppressWarnings(Array("org.wartremover.warts.While"))
      def f() = {
        while (true) {
        }
      }
    }
    assertEmpty(result)
  }
}
