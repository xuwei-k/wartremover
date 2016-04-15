package org.brianmckenna.wartremover
package warts

object Gakuzzzz extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._

    val UnapplyName: TermName = "unapply"
    val UnapplySeqName: TermName = "unapplySeq"
    val xmlSymbols = List(
      "scala.xml.Elem", "scala.xml.NamespaceBinding"
    ) // cannot do `map rootMirror.staticClass` here because then:
      //   scala.ScalaReflectionException: object scala.xml.Elem in compiler mirror not found.

    new u.Traverser {
      override def traverse(tree: Tree): Unit = {
        val synthetic = isSynthetic(u)(tree)
        tree match {
          // Ignore trees marked by SuppressWarnings
          case t if hasWartAnnotation(u)(t) =>
          // Ignore xml literals
          case Apply(Select(left, _), _) if xmlSymbols.contains(left.tpe.typeSymbol.fullName) =>
          // Ignore synthetic methods in companion objects
          case ModuleDef(mods, _, Template(parents, self, stats)) =>
            mods.annotations foreach { annotation =>
              traverse(annotation)
            }
            parents foreach { parent =>
              traverse(parent)
            }
            traverse(self)
            stats filter {
              case dd: DefDef if isSynthetic(u)(dd) =>
                false
              case _ =>
                true
            } foreach { stat =>
              traverse(stat)
            }
          case Literal(Constant(null)) =>
            u.error(tree.pos, "null is disabled")

            // https://twitter.com/xuwei_k/status/515058568815341568
            scala.concurrent.util.Unsafe.instance.getInt(0)
            super.traverse(tree)
          // Scala pattern matching outputs synthetic null.asInstanceOf[X]
          case ValDef(mods, _, _, _) if mods.hasFlag(Flag.MUTABLE) && synthetic =>
          // TODO: This ignores a lot
          case LabelDef(_, _, _) if synthetic =>
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
