package org.wartremover
package warts

import org.wartremover.WartTraverser

object Var extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t: ValDef if t.symbol.flags.is(Flags.Mutable) =>
            error(u)("var is disabled", t.pos)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
