package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.{ Enumeration => EnumerationWart }

class EnumerationTest extends ResultAssertions {
  @Test def `can't declare Enumeration classes` = {
    val result = WartTestTraverser(EnumerationWart) {
      class Color extends Enumeration {
        val Red = Value
        val Blue = Value
      }
    }
    assertError(result)("Enumeration is disabled - use case objects instead")
  }
  @Test def `can't declare Enumeration objects` = {
    val result = WartTestTraverser(EnumerationWart) {
      object Color extends Enumeration {
        val Red = Value
        val Blue = Value
      }
    }
    assertError(result)("Enumeration is disabled - use case objects instead")
  }
  @Test def `can use user-defined Enumeration traits` = {
    val result = WartTestTraverser(EnumerationWart) {
      trait Enumeration
      object Foo extends Enumeration
    }
    assertEmpty(result)
  }
  @Test def `Enumeration wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(EnumerationWart) {
      @SuppressWarnings(Array("org.wartremover.warts.Enumeration"))
      object Color extends Enumeration {
        val Red = Value
        val Blue = Value
      }
    }
    assertEmpty(result)
  }
}
