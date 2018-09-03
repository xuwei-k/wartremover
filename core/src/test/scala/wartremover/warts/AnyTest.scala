package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Any

class AnyTest extends ResultAssertions {
  private def foo: Any = 42

  @Test def `Any can't be inferred` = {
    val result = WartTestTraverser(Any) {
      val x = foo
      x
    }
    assertError(result)("Inferred type containing Any")
  }
  @Test def `Any wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Any) {
      @SuppressWarnings(Array("org.wartremover.warts.Any"))
      val x = foo
      x
    }
    assertEmpty(result)
  }
}
