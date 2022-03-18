package org.wartremover
package warts

object Enumeration extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case a: TypeTree if a.tpe =:= TypeRepr.of[scala.Enumeration] =>
            error(u)(tree.pos, "Enumeration is disabled - use case objects instead")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
