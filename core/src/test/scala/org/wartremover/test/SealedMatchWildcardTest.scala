package org.wartremover
package test

import org.wartremover.warts.SealedMatchWildcard
import org.scalatest.funsuite.AnyFunSuite

object SealedMatchWildcardTest {
  sealed trait A1
  case class A2(x: Int) extends A1
  case class A3(x: String) extends A1
}

class SealedMatchWildcardTest extends AnyFunSuite with ResultAssertions {
  test("can use Option") {
    def opt: Option[Int] = None

    val result = WartTestTraverser(SealedMatchWildcard) {
      def x1 = opt match {
        case Some(value) => 2
        case _ => 3
      }
    }
    assertEmpty(result)
  }

  test("disable wildcard") {
    import SealedMatchWildcardTest._
    def a1: A1 = A2(3)
    val result = WartTestTraverser(SealedMatchWildcard) {
      def x1 = a1 match {
        case A3(value) => 2
        case _ => 3
      }
    }
    assert(result.errors.size == 1)
  }
}
