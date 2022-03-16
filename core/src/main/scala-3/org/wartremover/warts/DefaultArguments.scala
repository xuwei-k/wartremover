package org.wartremover
package warts

object DefaultArguments extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case DefDef(name, params, _, _) if
              (name != "copy") &&
              params.flatMap(_.params).exists(p =>
                p.symbol.flags.is(Flags.HasDefault) &&
                !p.symbol.flags.is(Flags.Synthetic)
              ) =>
            error(u)(tree.pos, "Function has default arguments")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
