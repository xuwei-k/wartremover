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
          case Apply(Select(receiver, "=="), Literal(NullConstant()) :: Nil) =>
          case Apply(Select(receiver, "=="), _) if
            (receiver.tpe.typeSymbol == TypeRepr.of[Array].typeSymbol) || (receiver.tpe <:< TypeRepr.of[Iterator[Any]]) =>
            error(u)(tree.pos, "== is disabled, use sameElements instead")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
