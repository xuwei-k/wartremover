package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Unsafe

class CompanionTest extends ResultAssertions {
  @Test def `can use companion objects for case classes` = {
    val result = WartTestTraverser(Unsafe) {
      case class Foo(n: Int)
      object Foo
    }
    assertEmpty(result)
  }
  @Test def `can use companion objects for type aliases` = {
    val result = WartTestTraverser(Unsafe) {
      trait T[R]
      type T1 = String
      object T1 extends T[Unit]
    }
    assertEmpty(result)
  }
}
