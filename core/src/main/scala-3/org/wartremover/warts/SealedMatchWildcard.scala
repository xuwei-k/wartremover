package org.wartremover.warts

import org.wartremover.WartTraverser
import org.wartremover.WartUniverse

object SealedMatchWildcard extends WartTraverser {
  private[this] val exclude = Seq("scala.")

  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case Match(selector, xs :+ lastCase) if selector.tpe.typeSymbol.flags.is(Flags.Sealed) && xs.nonEmpty =>
            val tpe = selector.tpe.widen.show
            if (!exclude.exists(tpe.startsWith)) {
              val childrenSize = selector.tpe.typeSymbol.children.size
              if ((childrenSize - xs.size) < 4) {
                lastCase.pattern match {
                  case _: Wildcard if sourceCodeContains(lastCase, " case ") =>
                    error(lastCase.pattern.pos, s"don't use wildcard for sealed type ${tpe} ${childrenSize} ${xs.size}")
                  case _ =>
                }
              }
            }
            super.traverseTree(tree)(owner)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
