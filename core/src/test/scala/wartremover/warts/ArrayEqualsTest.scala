package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.ArrayEquals

class ArrayEqualsTest extends ResultAssertions {
  @Test def `Array == is disabled` = {
    val result = WartTestTraverser(ArrayEquals) {
      Array(1) == Array(1)
    }
    assertError(result)("== is disabled, use sameElements instead")
  }

  @Test def `Iterator == is disabled` = {
    val result = WartTestTraverser(ArrayEquals) {
      Iterator(1) == Iterator(1)
    }
    assertError(result)("== is disabled, use sameElements instead")
  }

  @Test def `Collections == is allowed` = {
    val result = WartTestTraverser(ArrayEquals) {
      List(1) == List(1)
    }
    assertEmpty(result)
  }

  @Test def `ArrayEquals wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(ArrayEquals) {
      @SuppressWarnings(Array("org.wartremover.warts.ArrayEquals"))
      def f = Array(1) == Array(1)
    }
    assertEmpty(result)
  }
}
