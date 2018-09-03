package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Unsafe

class ExistentialTest extends ResultAssertions {
  @Test def `can use existential values` = {
    val result = WartTestTraverser(Unsafe) {
      case class Name[A](value: String)
      def values(names: Name[_]*) =
        names map { n => n.value }
    }
    assertEmpty(result)
  }
}
