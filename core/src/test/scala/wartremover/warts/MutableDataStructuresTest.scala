package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.MutableDataStructures

class MutableDataStructuresTest extends ResultAssertions {
  @Test def `disable scala.collection.mutable._ when referenced` = {
    val result = WartTestTraverser(MutableDataStructures) {
      var x = scala.collection.mutable.HashMap("key" -> "value")
    }
    assertError(result)("scala.collection.mutable package is disabled")
  }
  @Test def `ignore immutable collections` = {
    val result = WartTestTraverser(MutableDataStructures) {
      var x = Map("key" -> "value")
    }
    assertEmpty(result)
  }
  @Test def `MutableDataStructures wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(MutableDataStructures) {
      @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
      var x = scala.collection.mutable.HashMap("key" -> "value")
    }
    assertEmpty(result)
  }
}
