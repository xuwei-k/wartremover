package org.wartremover
package warts

object AnyVal extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if hasWartAnnotation(tree) =>
          case a: Inferred if a.tpe =:= TypeRepr.of[AnyVal] =>
            error(u)("Inferred type containing AnyVal: AnyVal", tree.pos)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
