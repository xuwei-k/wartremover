package org.wartremover
package test


import org.wartremover.warts.Throw
import org.scalatest.funsuite.AnyFunSuite

class ThrowTest2 extends AnyFunSuite with ResultAssertions {
  test("throw is allowed in synthetic MatchError") {
    val result = WartTestTraverser(Throw) {
      val (a, b) = (1 to 10).partition(_ % 2 == 0)
      Option(2) match {
        case Some(1) =>
      }
    }
    assertEmpty(result)
  }
}
