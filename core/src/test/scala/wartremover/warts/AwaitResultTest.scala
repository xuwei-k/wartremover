package org.wartremover
package test

import org.scalatest.FunSuite
import org.wartremover.warts.AwaitResult
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class AwaitResultest extends FunSuite with ResultAssertions {
  test("can't use Await#result") {
    val f = Future.successful(42)
    val result = WartTestTraverser(AwaitResult) {
      Await.result(f, 1.second)
    }
    assertError(result)("Await#result is disabled")
  }
}
