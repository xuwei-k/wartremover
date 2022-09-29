package org.wartremover
package warts

object Copy extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    val scalaProduct = rootMirror.staticClass("scala.Product").toType
    new u.Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case Select(
                obj,
                TermName("copy")
              ) if obj.tpe <:< scalaProduct =>
            error(u)(tree.pos, "copy is disabled")
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
