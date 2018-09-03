package org.wartremover
package test

import org.junit.Test
import scala.annotation.tailrec
import org.wartremover.warts.Recursion

class RecursionTest extends ResultAssertions {

  @Test def `can't use recursion` = {
    val result = WartTestTraverser(Recursion) {
      def foo(x: Int): Int = foo(x)
    }
    assertError(result)("Unmarked recursion")
  }

  @Test def `can't use nested recursion` = {
    val result = WartTestTraverser(Recursion) {
      def foo(x: Int): Int = {
        def bar(y: Int): Int = foo(y)
        bar(1)
      }
    }
    assertError(result)("Unmarked recursion")
  }

  @Test def `can use non-recursion` = {
    val result = WartTestTraverser(Recursion) {
      def foo(x: Int): Int = x
    }
    assertEmpty(result)
  }

  @Test def `can use tail recursion` = {
    val result = WartTestTraverser(Recursion) {
      @tailrec def foo(x: Int): Int = if (x < 0) 1 else foo(x - 1)
    }
    assertEmpty(result)
  }

  @Test def `Recursion wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Recursion) {
      @SuppressWarnings(Array("org.wartremover.warts.Recursion"))
      def foo(x: Int): Int = foo(x)
    }
    assertEmpty(result)
  }
}
