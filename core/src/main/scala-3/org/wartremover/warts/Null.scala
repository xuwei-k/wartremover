package org.wartremover
package warts

object Null extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t if t.isExpr =>
            t.asExpr match {
              case '{ null == ($x: Any) } =>
              case '{ null != ($x: Any) } =>
              case '{ ($x: Any) == null } =>
              case '{ ($x: Any) != null } =>
              case '{ null eq ($x: AnyRef) } =>
              case '{ null ne ($x: AnyRef) } =>
              case '{ ($x: AnyRef) eq null } =>
              case '{ ($x: AnyRef) ne null } =>
              case '{ new scala.xml.Elem(null, $x1, $x2, $x3, $x4) } =>
              case '{ new scala.xml.NamespaceBinding(null, $x1, $x2) } =>
              case '{ null } =>
                error(u)(tree.pos, "null is disabled")
              case '{ ($x: Option[t]).orNull } =>
                error(u)(tree.pos, "Option#orNull is disabled")
              case _ =>
                super.traverseTree(tree)(owner)
            }
          case t @ ValDef(_, _, Some(Wildcard()))
              if t.symbol.flags.is(Flags.Mutable) && t.tpt.tpe <:< TypeRepr.of[AnyRef] =>
            error(u)(tree.pos, "null is disabled")

          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
