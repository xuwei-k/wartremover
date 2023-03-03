package org.wartremover
package test

import org.scalatest.funsuite.AnyFunSuite
import org.wartremover.warts.ApplyWeakConformance.asString

class ApplyWeakConformanceTest extends AnyFunSuite {
  test("ApplyWeakConformance") {
    assert(asString[Byte, Short] == Some("scala.Short"))
    assert(asString[Byte, Int] == Some("scala.Int"))
    assert(asString[Byte, Char] == None)
    assert(asString[Byte, Long] == Some("scala.Long"))
    assert(asString[Byte, Float] == Some("scala.Float"))
    assert(asString[Byte, Double] == Some("scala.Double"))
  }
}
