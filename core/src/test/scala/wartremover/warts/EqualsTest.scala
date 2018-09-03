package org.wartremover
package test

import org.junit.Assert.assertEquals
import org.junit.Test
import org.wartremover.warts.Equals

class EqualsTest extends ResultAssertions {
  @Test def `can't use == or != method on primitives` = {
    val result1 = WartTestTraverser(Equals) {
      5 == "foo"
    }
    assertError(result1)("== is disabled - use === or equivalent instead")

    val result2 = WartTestTraverser(Equals) {
      5 != "foo"
    }
    assertError(result2)("!= is disabled - use =/= or equivalent instead")
  }

  @Test def `can't use == or != on classes` = {
    class Foo(i: Int)

    val result1 = WartTestTraverser(Equals) {
      new Foo(5) == new Foo(4)
    }
    assertError(result1)("== is disabled - use === or equivalent instead")

    val result2 = WartTestTraverser(Equals) {
      new Foo(5) != new Foo(4)
    }
    assertError(result2)("!= is disabled - use =/= or equivalent instead")
  }

  @Test def `can't use == or != on case classes` = {
    case class Foo(i: Int)

    val result1 = WartTestTraverser(Equals) {
      Foo(5) == Foo(4)
    }
    assertError(result1)("== is disabled - use === or equivalent instead")

    val result2 = WartTestTraverser(Equals) {
      Foo(5) != Foo(4)
    }
    assertError(result2)("!= is disabled - use =/= or equivalent instead")

    val result3 = WartTestTraverser(Equals) {
      "abc" == Foo(4)
    }
    assertError(result3)("== is disabled - use === or equivalent instead")
  }

  @Test def `can't use equals` = {
    class Foo(i: Int)

    val result = WartTestTraverser(Equals) {
      new Foo(5).equals(new Foo(4))

      5.equals("foo")
    }
    assertErrors(result)("equals is disabled - use === or equivalent instead", 2)
  }

  @Test def `can't use overridden equals` = {
    class Foo(i:Int) {
      override def equals(obj: scala.Any) = false
    }

    val result = WartTestTraverser(Equals) {
      new Foo(1).equals(1)
    }
    assertError(result)("equals is disabled - use === or equivalent instead")
  }

  @Test def `can use custom equals` = {
    val result = WartTestTraverser(Equals) {
      java.util.Arrays.equals(Array(1), Array(1))
    }
    assertEmpty(result)
  }

  @Test def `can't use eq or ne` = {
    class Foo(i: Int)

    val result1 = WartTestTraverser(Equals) {
      new Foo(5) eq new Foo(4)
    }
    assertError(result1)("eq is disabled - use === or equivalent instead")

    val result2 = WartTestTraverser(Equals) {
      new Foo(5) ne new Foo(4)
    }
    assertError(result2)("ne is disabled - use =/= or equivalent instead")
  }

  @Test def `Equals wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Equals) {
      case class Foo(i: Int)
      @SuppressWarnings(Array("org.wartremover.warts.Equals"))
      val i = Foo(5) == Foo(4)
      @SuppressWarnings(Array("org.wartremover.warts.Equals"))
      val j = Foo(5) != Foo(4)
    }
    assertEmpty(result)
  }

  @Test def `Equals should work in synthetic lambdas` = {
    val result = WartTestTraverser(Equals) {
      Seq(1, 2, 3).exists(_ == 1)
      Seq(1, 2, 3).exists(n => n != 1)
    }
    assertEquals("result.errors",
      List(
      "[wartremover:Equals] == is disabled - use === or equivalent instead",
      "[wartremover:Equals] != is disabled - use =/= or equivalent instead"),
      result.errors)
  }

  @Test def `Equals should work in explicit lambdas` = {
    val result = WartTestTraverser(Equals) {
      Seq(1, 2, 3).exists(new Function1[Int, Boolean] { def apply(i: Int): Boolean = i == 1 })
      Seq(1, 2, 3).exists(new Function1[Int, Boolean] { def apply(i: Int): Boolean = i != 1 })
    }
    assertEquals("result.errors",
      List(
      "[wartremover:Equals] == is disabled - use === or equivalent instead",
      "[wartremover:Equals] != is disabled - use =/= or equivalent instead"),
      result.errors)
  }

}
