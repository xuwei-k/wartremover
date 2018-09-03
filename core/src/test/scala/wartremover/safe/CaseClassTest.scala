package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Unsafe

class CaseClassTest extends ResultAssertions {
  @Test def `case classes still work` = {
    val result = WartTestTraverser(Unsafe) {
      case class A(a: Int)
      case class B[X](a: X)
    }
    assertEmpty(result)
  }
  @Test def `vararg case classes still work` = {
    val result = WartTestTraverser(Unsafe) {
      case class A(a: Int*)
    }
    assertEmpty(result)
  }
}
