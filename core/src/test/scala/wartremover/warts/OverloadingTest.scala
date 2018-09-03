package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Overloading

class OverloadingTest extends ResultAssertions {
  @Test def `Overloading is disabled` = {
    val result = WartTestTraverser(Overloading) {
      class c {
        def f(i: Int) = {}
        def f(s: String) = {}
        def wait(s: String) = {}
      }
    }
    assertErrors(result)("Overloading is disabled", 3)
  }

  @Test def `Overriding is allowed` = {
    trait t {
      def print(x: String): Unit
      def print(x: Int): Unit = print(x.toString)
    }

    val result = WartTestTraverser(Overloading) {
      class c extends t {
        def print(x: String) = {}
        override def print(x: Int) = {}
      }
    }
    assertEmpty(result)
  }

  @Test def `Overloading wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Overloading) {
      @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
      class c {
        def f(i: Int) = {}
        def f(s: String) = {}
        def wait(s: String) = {}
      }
    }
    assertEmpty(result)
  }

  @Test def `Overloading wart obeys SuppressWarnings on defs` = {
    val result = WartTestTraverser(Overloading) {
      class c {
        def f(i: Int) = {}
        @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
        def f(s: String) = {}
      }
    }
    assertEmpty(result)
  }
}
