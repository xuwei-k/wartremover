package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.ExplicitImplicitTypes

class WartTraverserTest extends ResultAssertions {
  @Test def `isWartAnnotation should correctly look at the annotations of the accessed fields for accessors` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      class A {
        @SuppressWarnings(Array("org.wartremover.warts.ExplicitImplicitTypes"))
        implicit val foo = "should be suppressed"
      }
    }
    assertEmpty(result)
  }

  @Test def `hasTypeAscription should correctly look at the accessed fields for accessors` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      class A {
        implicit var foo: String = "Should not be reported"
      }
    }
    assertEmpty(result)
  }
}
