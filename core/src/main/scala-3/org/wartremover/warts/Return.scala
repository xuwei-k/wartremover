package org.wartremover
package warts

import org.wartremover.WartTraverser

object Return extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(u)(t) =>
          case t: Return =>
            error(u)(tree.pos, "return is disabled")
          case _ => super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
