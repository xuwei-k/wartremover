package org.wartremover
package warts

object TryPartial extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(u)(t) =>
          case t if t.isExpr =>
            t.asExpr match {
              case '{ ($x: scala.util.Try[t]).get } =>
                error(u)(tree.pos, "Try#get is disabled")
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
