package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.NonUnitStatements

class NonUnitStatementsTest extends ResultAssertions {
  @Test def `non-unit statements are disabled` = {
    val result = WartTestTraverser(NonUnitStatements) {
      1
      2
    }
    assertError(result)("Statements must return Unit")
  }
  @Test def `Extending a class with multiple parameter lists doesn't fail` = {
    val result = WartTestTraverser(NonUnitStatements) {
      class A(x: Int)(y: Int)(z: Int)
      class B(x: Int)(y: Int)(z: Int) extends A(x)(y)(z)
    }
    assertEmpty(result)
  }
  @Test def `XML literals don't fail` = {
    val result = WartTestTraverser(NonUnitStatements) {
      val a = 13
      <x>{a}</x>
    }
    assertEmpty(result)
  }
  @Test def `&+ method but not xml` = {
    class A {
      def &+(i: Int): A = this
    }
    val result = WartTestTraverser(NonUnitStatements) {
      val a = new A
      a &+ 42
      a
    }
    assertError(result)("Statements must return Unit")
  }

  @Test def `NonUnitStatements wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(NonUnitStatements) {
      @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
      val foo = {
        1
        2
      }
    }
    assertEmpty(result)
  }
}
