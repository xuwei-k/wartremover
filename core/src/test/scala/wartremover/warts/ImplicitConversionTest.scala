package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.ImplicitConversion

class ImplicitConversionTest extends ResultAssertions {
  @Test def `Implicit conversion is disabled` = {
    val result = WartTestTraverser(ImplicitConversion) {
      class c {
        implicit def int2Array(i: Int): Array[String] = Array.fill(i)("?")
      }
    }
    assertError(result)("Implicit conversion is disabled")
  }

  @Test def `Non-public implicit conversion is enabled` = {
    val result = WartTestTraverser(ImplicitConversion) {
      class c {
        protected implicit def int2Array(i: Int): Array[String] = Array.fill(i)("?")
      }
    }
    assertEmpty(result)
  }

  @Test def `Implicit evidence constructor is enabled` = {
    val result = WartTestTraverser(ImplicitConversion) {
      implicit def ordering[A]: Ordering[A] = ???
      implicit def ordering2[A](implicit ev : Ordering[A]) : Ordering[A] = ???
      implicit def ordering3[A : Ordering] : Ordering[A] = ???
    }
    assertEmpty(result)
  }

  @Test def `ImplicitConversion wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(ImplicitConversion) {
      class c {
        @SuppressWarnings(Array("org.wartremover.warts.ImplicitConversion"))
        implicit def int2Array(i: Int): Array[String] = Array.fill(i)("?")
      }
    }
    assertEmpty(result)
  }
}
