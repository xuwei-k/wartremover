package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Throw

class ThrowTest extends ResultAssertions {
  @Test def `throw is disabled` = {
    val result = WartTestTraverser(Throw) {
      def foo(n: Int): Int = throw new IllegalArgumentException("bar")
    }
    assertError(result)("throw is disabled")
  }

  @Test def `throw is disabled for non-synthetic MatchErrors` = {
    val result = WartTestTraverser(Throw) {
      def foo(n: Int): Int = throw new MatchError("bar")
    }
    assertError(result)("throw is disabled")
  }

  @Test def `throw is allowed in synthetic Product.productElement` = {
    val result = WartTestTraverser(Throw) {
      case class Foo(i: Int)
    }
    assertEmpty(result)
  }

  @Test def `Throw wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Throw) {
      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def foo(n: Int): Int = throw new IllegalArgumentException("bar")
    }
    assertEmpty(result)
  }
}
