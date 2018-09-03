package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Return

class ReturnTest extends ResultAssertions {
  @Test def `local return is disabled` = {
    val result = WartTestTraverser(Return) {
      def foo(n:Int): Int = return n + 1
    }
    assertError(result)("return is disabled")
  }
  @Test def `nonlocal return is disabled` = {
    val result = WartTestTraverser(Return) {
      def foo(ns: List[Int]): Any = ns.map(n => return n + 1)
    }
    assertError(result)("return is disabled")
  }
  @Test def `Return wart is disabled` = {
    val result = WartTestTraverser(Return) {
      @SuppressWarnings(Array("org.wartremover.warts.Return"))
      def foo(n:Int): Int = return n + 1
      @SuppressWarnings(Array("org.wartremover.warts.Return"))
      def bar(ns: List[Int]): Any = ns.map(n => return n + 1)
    }
    assertEmpty(result)
  }
}
