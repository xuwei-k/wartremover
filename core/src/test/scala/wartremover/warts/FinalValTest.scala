package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.FinalVal

class FinalValTest extends ResultAssertions {
  @Test def `final val is disabled` = {
    val result = WartTestTraverser(FinalVal) {
      class c {
        final val v = 1
      }
    }
    assertError(result)("final val is disabled - use non-final val or final def or add type ascription")
  }

  @Test def `final val alternatives are enabled` = {
    val result = WartTestTraverser(FinalVal) {
      class c {
        val v = 1
        final def v2 = 1
        final val v3: Int = 1
      }
    }
    assertEmpty(result)
  }

  @Test def `FinalVal wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(FinalVal) {
      class c {
        @SuppressWarnings(Array("org.wartremover.warts.FinalVal"))
        final val v = 1
      }
    }
    assertEmpty(result)
  }
}
