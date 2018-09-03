package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.LeakingSealed

class LeakingSealedTest extends ResultAssertions {
  @Test def `Descendants of a sealed type must be final or sealed` = {
    val result = WartTestTraverser(LeakingSealed) {
      sealed trait t
      class c extends t
    }
    assertError(result)("Descendants of a sealed type must be final or sealed")
  }

  @Test def `Final or sealed descendants of a sealed type are allowed` = {
    val result = WartTestTraverser(LeakingSealed) {
      sealed trait t
      final class c extends t
      sealed trait tt extends t
    }
    assertEmpty(result)
  }

  @Test def `LeakingSealed wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(LeakingSealed) {
      sealed trait t
      @SuppressWarnings(Array("org.wartremover.warts.LeakingSealed"))
      class c extends t
    }
    assertEmpty(result)
  }
}
