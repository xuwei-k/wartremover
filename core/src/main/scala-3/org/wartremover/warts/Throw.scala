package org.wartremover
package warts

object Throw extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t if t.isExpr =>
            t.asExpr match {
              case '{ throw $x } =>
                val notGeneratedCode =
                  try {
                    t.pos.sourceCode.exists(_.contains("throw"))
                  } catch {
                    case _: StringIndexOutOfBoundsException =>
                      false
                  }
                if (notGeneratedCode) {
                  error(u)(tree.pos, "throw is disabled")
                } else {
                  super.traverseTree(tree)(owner)
                }
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
