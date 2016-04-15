package org.brianmckenna.wartremover
package test

import org.scalatest.FunSuite

import org.brianmckenna.wartremover.warts.Gakuzzzz

class GakuzzzzTest extends FunSuite {
  test("JVM crash if use `null`") {
    val result = WartTestTraverser(Gakuzzzz) {
      println(null)
    }
  }
}
