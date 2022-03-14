package org.wartremover
package warts

import org.wartremover.WartTraverser

object TestWart1 extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case a: TypeTree if a.tpe =:= TypeRepr.of[Matchable] =>
            report.warning("inferred Matchable", tree.pos)
          case a: TypeTree if a.tpe =:= TypeRepr.of[Any] =>
            report.warning("inferred Any", tree.pos)
          case a: TypeTree if a.tpe =:= TypeRepr.of[AnyVal] =>
            report.warning("Inferred type containing AnyVal: AnyVal", tree.pos)
          case a: TypeTree if a.tpe <:< TypeRepr.of[scala.App] =>
            report.warning("do not extends scala.App", tree.pos)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
