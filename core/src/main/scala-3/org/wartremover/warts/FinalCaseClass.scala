package org.wartremover
package warts

object FinalCaseClass extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(u)(t) =>
          case t: ClassDef if !t.symbol.flags.is(Flags.Synthetic) && t.symbol.isClassDef =>
            if (t.symbol.flags.is(Flags.Case) && !(t.symbol.flags.is(Flags.Final) || t.symbol.flags.is(Flags.Sealed))) {
              error(u)(tree.pos, "case classes must be final")
            }
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
