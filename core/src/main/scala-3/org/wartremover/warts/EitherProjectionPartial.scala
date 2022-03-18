package org.wartremover
package warts

import org.wartremover.WartTraverser

object EitherProjectionPartial extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t if t.isExpr =>
            t.asExpr match {
              case '{ (${ a }: Either.RightProjection[left, right]).get } =>
                error(u)(t.pos, "RightProjection#get is disabled - use RightProjection#toOption instead")
              case '{ (${ a }: Either.LeftProjection[left, right]).get } =>
                error(u)(t.pos, "LeftProjection#get is disabled - use LeftProjection#toOption instead")
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
