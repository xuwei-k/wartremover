package org.wartremover.warts

import org.wartremover.WartTraverser
import org.wartremover.WartUniverse

object ExplicitImplicitImport extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    new Traverser {
      override def traverse(tree: Tree) = {
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case i: Import if i.expr.symbol.isModule =>
            val t = i.expr.symbol.asModule.moduleClass.typeSignature
            i.selectors.foreach { s =>
              if (t.member(s.name).isImplicit) {
                warning(u)(tree.pos, "Don't import implicit values with explicit name")
              }
            }
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
