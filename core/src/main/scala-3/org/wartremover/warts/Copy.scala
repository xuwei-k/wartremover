package org.wartremover
package warts

object Copy extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case Select(obj, "copy") if obj.tpe <:< TypeRepr.of[scala.Product] =>
            error(tree.pos, "copy is disabled")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
