package org.wartremover
package test

import org.wartremover.warts.ContextBoundsUsing
import org.scalatest.funsuite.AnyFunSuite

class ContextBoundsUsingTest extends AnyFunSuite with ResultAssertions {
  private class MyTypeClass[A]

  private class Obj {
    def f[A: MyTypeClass](a: A): Unit = ()
  }
  private def obj: Obj = ???
  private implicit def instance: MyTypeClass[Int] = ???

  test("implicitで受け渡してる場合") {
    val result = WartTestTraverser(ContextBoundsUsing) {
      obj.f(2)
    }
    assertEmpty(result)
  }
  test("明示的に渡してるのにusing付与してない") {
    val result = WartTestTraverser(ContextBoundsUsing) {
      obj.f(3)(instance)
    }
    assertError(result)("context boundsに明示的に引数渡してるのにusing付与してません")
  }
  test("明示的に渡して、かつusing付与") {
    val result = WartTestTraverser(ContextBoundsUsing) {
      obj.f(4)(using instance)
    }
    assertEmpty(result)
  }
}
