package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Option2Iterable

class Option2IterableTest extends ResultAssertions {

  @Test def `can't use Option.option2Iterable with Some` = {
    val result = WartTestTraverser(Option2Iterable) {
      println(Iterable(1).flatMap(Some(_)))
    }
    assertError(result)("Implicit conversion from Option to Iterable is disabled - use Option#toList instead")
  }
  @Test def `can't use Option.option2Iterable with None` = {
    val result = WartTestTraverser(Option2Iterable) {
      println(Iterable(1).flatMap(_ => None))
    }
    assertError(result)("Implicit conversion from Option to Iterable is disabled - use Option#toList instead")
  }
  @Test def `can't use Option.option2Iterable when zipping Options` = {
    val v = scala.util.Properties.versionNumberString
    val result = WartTestTraverser(Option2Iterable) {
      println(Option(1) zip Option(2))
    }
    if (v.matches("2\\.1[012].*")) {
      assertErrors(result)("Implicit conversion from Option to Iterable is disabled - use Option#toList instead", 2)
    } else {
      // https://github.com/scala/scala/blob/v2.13.0-M4/src/library/scala/Option.scala#L321
      assertEmpty(result)
    }
  }
  @Test def `doesn't detect user defined option2Iterable functions` = {
    def option2Iterable[A](o: Option[A]): Iterable[A] = o.toIterable
    val result = WartTestTraverser(Option2Iterable) {
      println(option2Iterable(Some(1)))
    }
    assertEmpty(result)
  }
  @Test def `Option2Iterable wart obeys SuppressWarnings` = {
    val result = WartTestTraverser(Option2Iterable) {
      @SuppressWarnings(Array("org.wartremover.warts.Option2Iterable"))
      val foo = {
        println(Iterable(1).flatMap(Some(_)))
      }
    }
    assertEmpty(result)
  }
}
