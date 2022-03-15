package org.wartremover
package warts

import org.wartremover.WartTraverser

object ScalaApp extends WartTraverser {
  private def message = "Don't use scala.App. https://docs.scala-lang.org/scala3/book/methods-main-methods.html"
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case a: TypeTree if a.tpe =:= TypeRepr.of[scala.App] =>
            error(u)(message, tree.pos)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
