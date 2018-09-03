package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Serializable

class SerializableTest extends ResultAssertions {
  @Test def `Serializable can't be inferred` = {
    val result = WartTestTraverser(Serializable) {
      List((1, 2, 3), (1, 2))
    }
    assertError(result)("Inferred type containing Serializable")
  }
  @Test def `Serializable wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Serializable) {
      @SuppressWarnings(Array("org.wartremover.warts.Serializable"))
      val foo = List((1, 2, 3), (1, 2))
    }
    assertEmpty(result)
  }
}
