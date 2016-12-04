package org.wartremover
package warts

object AwaitResult extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    val result = typeOf[scala.concurrent.Await.type].member(newTermName("result"))
    new u.Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          case rt: RefTree if rt.symbol == result =>
            u.error(tree.pos, "Await#result is disabled")
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
