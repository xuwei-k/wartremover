package org.wartremover
package warts

object AnyVal extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      def traverse(tree: Tree): Unit = {
        tree match {
          case a: Inferred if a.tpe =:= TypeRepr.of[AnyVal] =>
            error(u)("Inferred type containing AnyVal: AnyVal", tree.pos)
          case _ =>
        }
      }
    }
  }
}
