package org.wartremover
package warts

import org.wartremover.WartTraverser

object Var extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*

      private def notXmlTypes(t: ValDef): Boolean = {
        !List(
          TypeRepr.of[scala.xml.MetaData],
          TypeRepr.of[scala.xml.NamespaceBinding],
        ).exists(
          t.tpt.tpe =:= _
        )
      }

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t: ValDef if t.symbol.flags.is(Flags.Mutable) && !t.symbol.flags.is(Flags.Synthetic) && notXmlTypes(t) =>
            error(u)(t.pos, "var is disabled")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
