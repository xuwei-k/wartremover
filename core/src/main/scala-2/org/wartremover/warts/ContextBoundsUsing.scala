package org.wartremover
package warts

object ContextBoundsUsing extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    new u.Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case _ if hasWartAnnotation(u)(tree) =>
          case Apply(method, args) if method.symbol.isMethod =>
            val m = method.symbol.asMethod
            val contextParamTypeConstructors: Seq[Type] =
              m.paramLists.lastOption.toList.flatten
                .withFilter(_.isImplicit)
                .withFilter(_.name.toString.startsWith("evidence$"))
                .map(_.info.dealias.typeConstructor)

            if (false) {
              println(contextParamTypeConstructors)
            }
            args
              .filter(arg => contextParamTypeConstructors.exists(arg.tpe.dealias.typeConstructor =:= _))
              .filter(arg => (arg.pos.end - arg.pos.start) > 0)
              .filterNot { arg =>
                val p = arg.pos
                arg.pos.source.content.slice(method.pos.end, p.start).mkString.contains("using")
              }
              .foreach { arg =>
                error(u)(arg.pos, "context boundsに明示的に引数渡してるのにusing付与してません")
                if (false) {
                  val p = arg.pos
                  Seq(
                    args.size,
                    args,
                    "method" -> (method.pos.end - method.pos.start),
                    "method.pos.end" -> (method.pos.end),
                    "line" -> p.line,
                    "isRange" -> p.isRange,
                    "isTransparent" -> p.isTransparent,
                    "isOpaqueRange" -> p.isOpaqueRange,
                    "start" -> p.start,
                    "end" -> p.end,
                    "range" -> (p.end - p.start),
                    "source" -> p.source,
                    "column" -> p.column,
                  ).foreach(println)

                  println("")
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
