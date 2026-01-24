package org.wartremover
package warts

object ArrayEquals extends WartTraverser {
  private val types: Set[String] = Set(
    "scala.Array",
    "scala.collection.Iterator",
  )

  def apply(u: WartUniverse): u.Traverser = {
    new u.FusionTraverser(this) {
      import q.reflect.*
      override def traverseApply(tree: Apply, owner: Symbol) = {
        tree match {
          case _ if sourceCodeNotContains(tree, "==") =>
            TraverseState.Stop
          case t if hasWartAnnotation(t) =>
            TraverseState.Stop
          case Apply(Select(x1, "=="), Literal(NullConstant()) :: Nil) =>
            TraverseState.Continue
          case Apply(Select(x1, "=="), _ :: Nil) if x1.tpe.baseClasses.exists(t => types(t.fullName)) =>
            error(tree.pos, "== is disabled")
            TraverseState.Stop
          case _ =>
            TraverseState.Continue
        }
      }
    }
  }
}
