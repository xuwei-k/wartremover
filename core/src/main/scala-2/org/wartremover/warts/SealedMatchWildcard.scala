package org.wartremover.warts

import org.wartremover.WartTraverser
import org.wartremover.WartUniverse
import scala.annotation.nowarn

object SealedMatchWildcard extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    new Traverser {
      @nowarn("msg=lineContent")
      override def traverse(tree: Tree): Unit = {
        tree match {
          case t if hasWartAnnotation(u)(t) =>
          case Match(selector, cases) =>
            if (!isSynthetic(u)(tree) && !selector.tpe.typeSymbol.fullName.startsWith("scala.")) {
              val sym = selector.tpe.typeSymbol
              if (sym.isClass && sym.asClass.isSealed) {
                cases.lastOption.map(_.pat).foreach {
                  case t @ Ident(_) if t.pos.lineContent.contains(" case ") =>
                    warning(u)(t.pos, "sealedな型に対するmatchの最後のcaseは全てにmatchするpatternを書かないでください")
                  case _ =>
                }
              }
            }
            super.traverse(tree)
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
