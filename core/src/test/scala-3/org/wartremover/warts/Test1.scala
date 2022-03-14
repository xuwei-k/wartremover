package org.wartremover.warts

import org.wartremover.WartTraverser
import org.wartremover.test.WartTestTraverser

object Test1 {
  def main(args: Array[String]): Unit = {
    val result = WartTestTraverser(TestWart1){
      List(2, IArray(3))
      List(1, "a")
    }
    result.errors.foreach(println)
    result.warnings.foreach(println)
    assert(result.warnings.nonEmpty)
    val res2 = WartTestTraverser(TestWart1){
      object Foo extends scala.App
    }
    assert(res2.warnings.nonEmpty)
    assert(res2.warnings.contains("do not extends scala.App"))
  }
}
