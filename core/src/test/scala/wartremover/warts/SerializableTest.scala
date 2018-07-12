package org.wartremover
package test

import org.scalatest.FunSuite

import org.wartremover.warts.Serializable

class SerializableTest extends FunSuite with ResultAssertions {
  test("issue 59") {
    trait A
    case class A1() extends A
    case class A2() extends A
    val result = WartTestTraverser(Serializable) {
      List(A1(), A2())
    }
    assertError(result)("Inferred type containing Serializable")
  }
  test("Serializable can't be inferred") {
    val result = WartTestTraverser(Serializable) {
      List((1, 2, 3), (1, 2))
    }
    assertError(result)("Inferred type containing Serializable")
  }
  test("Serializable wart obeys SuppressWarnings") {
    val result = WartTestTraverser(Serializable) {
      @SuppressWarnings(Array("org.wartremover.warts.Serializable"))
      val foo = List((1, 2, 3), (1, 2))
    }
    assertEmpty(result)
  }
}
