package org.wartremover
package wart

object InferOrTypeLeastUpperBound extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      private[this] val forbidden = List(
        TypeRepr.of[Matchable],
        TypeRepr.of[Product],
        TypeRepr.of[Serializable],
        TypeRepr.of[Any],
        TypeRepr.of[Nothing],
        TypeRepr.of[AnyRef]
      )
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if hasWartAnnotation(tree) =>
          case a: Inferred =>
            a.tpe match {
              case t: OrType =>
                val left = t.left.show
                val right = t.right.show
                implicit val ctx = q.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
                val lub = t.asInstanceOf[dotty.tools.dotc.core.Types.Type].widenUnion.asInstanceOf[TypeRepr]
                if (forbidden.exists(_ =:= lub)) {
                  error(tree.pos, s"`${left} | ${right}` で ${lub.show} に推論されています")
                }
                super.traverseTree(tree)(owner)
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
