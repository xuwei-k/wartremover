package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.DefaultArguments

class DefaultArgumentsTest extends ResultAssertions {
  @Test def `Default arguments can't be used` = {
    val result = WartTestTraverser(DefaultArguments) {
      def x(y: Int = 4) = y
    }
    assertError(result)("Function has default arguments")
  }
  @Test def `Default arguments wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(DefaultArguments) {
      @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
      def x(y: Int = 4) = y
    }
    assertEmpty(result)
  }
}
