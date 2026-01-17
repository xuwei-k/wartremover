package org.wartremover
package warts

import scala.meta.Term
import scala.meta.contrib.XtensionTreeOps

object CollectHeadOption extends WartTraverser {
  private def checkScalameta(src: String): Boolean = {
    implicitly[scala.meta.parsers.Parse[scala.meta.Stat]]
      .apply(
        scala.meta.Input.String(src),
        scala.meta.dialects.Scala3
      )
      .toOption
      .exists { metaTree =>
        import scala.meta.Term
        metaTree.collectFirst {
          case Term.Select(
                Term.Apply.After_4_6_0(
                  Term.Select(_, Term.Name("collect")),
                  Term.ArgClause(_ :: Nil, _)
                ),
                Term.Name("headOption")
              ) =>
            ()
        }.isEmpty
      }
  }

  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if tree.pos.sourceCode.fold(false)(checkScalameta) =>
          case _ if sourceCodeNotContains(tree, "collect") || sourceCodeNotContains(tree, "headOption") =>
          case t if hasWartAnnotation(t) =>
          case t if t.isExpr =>
            t.asExpr match {
              case '{ ($x: collection.Iterable[t1]).collect($f).headOption } =>
                error(t.pos, "you can use collectFirst instead of collect.headOption")
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
