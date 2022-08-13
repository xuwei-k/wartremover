package org.wartremover
package warts

object NoSealedMatch extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    new Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case t if hasWartAnnotation(u)(t) =>
          case Match(selector, cases) =>
            if (!isSynthetic(u)(tree) && !selector.tpe.typeSymbol.fullName.startsWith("scala.")) {
              val sym = selector.tpe.typeSymbol
              def existsTypePattern =
                cases.iterator.map(_.pat).exists {
                  case Typed(_, caseType) => caseType.tpe <:< selector.tpe
                  case _ => false
                }

              if (sym.isAbstract && sym.isClass && !sym.asClass.isSealed && existsTypePattern) {
                warning(u)(selector.pos, s"no-sealed type match ${sym.fullName}")
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
