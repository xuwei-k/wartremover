package org.wartremover
package warts

import org.wartremover.WartTraverser

object TestWart1 extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
