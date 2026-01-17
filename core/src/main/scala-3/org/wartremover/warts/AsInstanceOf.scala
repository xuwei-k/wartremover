package org.wartremover
package warts

object AsInstanceOf extends WartTraverser {
  override def check(source: String) = {
    if (source.contains("asInstanceOf")) {
      "continue"
    } else {
      "skip"
    }
  }

  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t if t.isExpr && sourceCodeContains(t, "asInstanceOf") =>
            t.asExpr match {
              case '{ ($x: t1).asInstanceOf[t2] } =>
                error(tree.pos, "asInstanceOf is disabled")
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
