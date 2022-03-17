package org.wartremover
package warts

import org.wartremover.WartTraverser

object ArrayEquals extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t if t.isExpr =>
            t.asExpr match {
              case '{ ($x1: Array[t]) == null} =>
              case '{ ($x1: Array[t]) == ($x2: Any)} =>
                error(u)(tree.pos, "== is disabled, use sameElements instead")
              case '{ ($x1: Iterator[t]) == ($x2: Any)} =>
                error(u)(tree.pos, "== is disabled, use sameElements instead")
              case _ =>
                super.traverseTree(tree)(owner)
            }
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
