package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.FinalCaseClass

class FinalCaseClassTest extends ResultAssertions {
  @Test def `can't declare nonfinal case classes` = {
    val result = WartTestTraverser(FinalCaseClass) {
      case class Foo(i: Int)
    }
    assertError(result)("case classes must be final")
  }

  @Test def `can declare final case classes` = {
    val result = WartTestTraverser(FinalCaseClass) {
      final case class Foo(i: Int)
    }
    assertEmpty(result)
  }

  @Test def `can declare nonfinal sealed case classes` = {
    val result = WartTestTraverser(FinalCaseClass) {
      sealed case class Foo(i: Int)
    }
    assertEmpty(result)
  }

  @Test def `can declare nonfinal regular classes` = {
    val result = WartTestTraverser(FinalCaseClass) {
      class Foo(i: Int)
    }
    assertEmpty(result)
  }

  @Test def `can declare nonfinal case classes inside other classes` = {
    val result = WartTestTraverser(FinalCaseClass) {
      class Outer {
        case class Foo(i: Int)
      }
    }
    assertEmpty(result)
  }

  @Test def `FinalCaseClass wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(FinalCaseClass) {
      @SuppressWarnings(Array("org.wartremover.warts.FinalCaseClass"))
      case class Foo(i: Int)
    }
    assertEmpty(result)
  }
}
