package org.wartremover
package warts

import dotty.tools.dotc.ast.tpd.InferredTypeTree

object FinalVal extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t: ValDef if t.symbol.flags.is(Flags.Final) =>
            t.tpt.tpe match {
              case _: ConstantType =>
                error(u)(tree.pos, "final val is disabled - use non-final val or final def or add type ascription")
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
