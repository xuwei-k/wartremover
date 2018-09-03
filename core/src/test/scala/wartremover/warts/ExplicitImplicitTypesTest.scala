package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.ExplicitImplicitTypes

class ExplicitImplicitTypesTest extends ResultAssertions {
  @Test def `can't declare implicit vals without a type ascription` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      implicit val foo = 5
    }
    assertError(result)("implicit definitions must have an explicit type ascription")
  }

  @Test def `can't declare implicit defs without a type ascription` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      implicit def foo = 5
      implicit def bar[A] = 5
      implicit def baz(i: Int) = 5
      implicit def qux[I](i: I) = 5
    }
    assertErrors(result)("implicit definitions must have an explicit type ascription", 4)
  }

  @Test def `can declare implicit classes` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      implicit class Foo(i : Int) {
        def bar = 2
      }
    }
    assertEmpty(result)
  }

  @Test def `can declare implicit vals with a type ascription` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      implicit val foo: Int = 5
      implicit var bar: Int = 5
    }
    assertEmpty(result)
  }

  @Test def `can declare implicit defs with a type ascription` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      implicit def foo: Int = 5
      implicit def bar[A]: Int = 5
      implicit def baz(i: Int): Int = 5
      implicit def qux[I](i: I): Int = 5
    }
    assertEmpty(result)
  }

  @Test def `can declare implicit arguments` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      Option(1).map { implicit i =>
        i
      }
    }
    assertEmpty(result)
  }

  @Test def `can declare non-implicit vals` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      val foo = 5
    }
    assertEmpty(result)
  }

  @Test def `can declare non-implicit defs` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      def foo = 5
    }
    assertEmpty(result)
  }

  @Test def `can declare backticked implicit defs` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      implicit def `foo`: Int = 5
      implicit val `foobar`: String = "5"
      implicit def `foo bar`: Long = 5L
    }
    assertEmpty(result)
  }

  @Test def `ExplicitImplicitTypes wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(ExplicitImplicitTypes) {
      @SuppressWarnings(Array("org.wartremover.warts.ExplicitImplicitTypes"))
      implicit val foo = 5

      @SuppressWarnings(Array("org.wartremover.warts.ExplicitImplicitTypes"))
      implicit def bar = 5
    }
    assertEmpty(result)
  }
}
