package org.wartremover
package warts

object TryPartial extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if sourceCodeNotContains(tree, "get") =>
          case t if hasWartAnnotation(t) =>
          case Select(t, "get") if t.tpe.baseClasses.exists(_.fullName == "scala.util.Try") =>
            error(t.pos, "Try#get is disabled")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
