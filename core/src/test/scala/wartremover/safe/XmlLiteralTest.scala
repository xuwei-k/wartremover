package org.wartremover
package test

import org.junit.Test

import org.wartremover.warts.Unsafe

class XmlLiteralTest extends ResultAssertions {
  @Test def `can use xml literals` = {
    val result = WartTestTraverser(Unsafe) {
      val x = <foo />
    }
    assertEmpty(result)
  }
  @Test def `can use attributes in xml literals` = {
    val result = WartTestTraverser(Unsafe) {
      <foo bar="baz" />
    }
    assertEmpty(result)
  }
  @Test def `can use xmlns attrib in XML literals` = {
    val result = WartTestTraverser(Unsafe) {
      <x xmlns="y"/> // this one has special meaning
    }
    assertEmpty(result)
  }
}
