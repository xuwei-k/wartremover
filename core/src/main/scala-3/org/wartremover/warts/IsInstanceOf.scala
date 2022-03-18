package org.wartremover
package warts

object IsInstanceOf extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t if t.isExpr =>
            t.asExpr match {
              case '{ ($x: t1).isInstanceOf[t2] } =>
                val notGeneratedCode = try {
                  t.pos.sourceCode.exists(_.contains("isInstanceOf"))
                } catch {
                  case _: StringIndexOutOfBoundsException =>
                    false
                }

                if (notGeneratedCode) {
                  error(u)(tree.pos, "isInstanceOf is disabled")
                } else {
                  super.traverseTree(tree)(owner)
                }
              case _ =>
                super.traverseTree(tree)(owner)
            }
          case _: TypedOrTest =>
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
