package org.brianmckenna.wartremover
package test

import org.scalatest.FunSuite
import org.brianmckenna.wartremover.warts.InfiniteLoop

class InfiniteLoopTest extends FunSuite {
  test("infinite loop") {
    trait Foo { def bar: Int }
    val result = WartTestTraverser(InfiniteLoop) {
      new Foo {
        override def bar = bar
      }
    }
    expectResult(List("method bar does nothing other than call itself recursively"), "result.errors")(result.errors)
  }
}
