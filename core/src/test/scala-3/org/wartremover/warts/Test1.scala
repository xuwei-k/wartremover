package org.wartremover.warts

import org.wartremover.WartTraverser
import org.wartremover.test.WartTestTraverser

object Test1 {
  def main(args: Array[String]): Unit = {
    val result = WartTestTraverser(TestWart1){
      List(1, "a")
    }
    println(result)
    assert(result.warnings.nonEmpty)
  }
}
