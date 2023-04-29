package org.wartremover
package test

import org.wartremover.warts.UnsoundEqual
import org.scalatest.funsuite.AnyFunSuite

class UnsoundEqualTest extends AnyFunSuite with ResultAssertions {
  test("disable ==") {
    val res1 = WartTestTraverser(UnsoundEqual) {
      UnsoundEqualTest.a1 == UnsoundEqualTest.b1
    }
    assertError(res1)(
      "unsound equal `java.lang.Object`. `org.wartremover.test.UnsoundEqualTest.A1 == org.wartremover.test.UnsoundEqualTest.B1`"
    )

    val res2 = WartTestTraverser(UnsoundEqual) {
      UnsoundEqualTest.a1 == IArray("a")
    }
    assertError(res2)(
      "unsound equal `scala.Any`. `org.wartremover.test.UnsoundEqualTest.A1 == scala.IArray$package.IArray[java.lang.String]`"
    )
  }
  test("allow ==") {
    val result = WartTestTraverser(UnsoundEqual) {
      UnsoundEqualTest.a1 == UnsoundEqualTest.a2
      UnsoundEqualTest.a1 == UnsoundEqualTest.a3
      UnsoundEqualTest.a2 == UnsoundEqualTest.a3
    }
    assertEmpty(result)
  }
}

object UnsoundEqualTest {
  class A1
  class A2 extends A1
  class A3 extends A1
  class B1

  def a1: A1 = new A1
  def a2: A2 = new A2
  def a3: A3 = new A3
  def b1: B1 = new B1
}
