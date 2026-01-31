package org.wartremover
package warts

object ForeachEntry extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      private val mapSymbol = Symbol.requiredClass("scala.collection.Map")

      val cache = {
        import scala.jdk.CollectionConverters.*
        new java.util.IdentityHashMap[SourceFile, Boolean]().asScala
      }

      private def existsForDoTree(src: SourceFile): Boolean = cache.getOrElseUpdate(
        src, {
          import dotty.tools.dotc.core.Contexts.Context
          import dotty.tools.dotc.parsing.Parsers.Parser
          import dotty.tools.dotc.ast.untpd.ForDo
          import dotty.tools.dotc.ast.untpd.GenFrom
          import dotty.tools.dotc.ast.untpd.Tuple
          import dotty.tools.dotc.ast.untpd
          import scala.quoted.runtime.impl.QuotesImpl

          given c: Context = q.asInstanceOf[QuotesImpl].ctx
          val untpdTree = c.compilationUnit.untpdTree

          untpd.existsSubTree(untpdTree) {
            case f: ForDo =>
              f.enums.exists {
                case GenFrom(Tuple(_ :: _ :: Nil), _, _) =>
                  true
                case _ =>
                  false
              }
            case _ =>
              false
          }
        }
      )

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _
              if (sourceCodeNotContains(tree, "foreach") && !existsForDoTree(
                tree.pos.sourceFile
              )) || sourceCodeNotContains(tree, "for") =>
          case t if hasWartAnnotation(t) =>
          case Apply(
                TypeApply(Select(x, "foreach"), _),
                Block(
                  List(
                    DefDef(
                      _,
                      _,
                      _,
                      Some(
                        Match(
                          _,
                          cases
                        )
                      )
                    )
                  ),
                  _: Closure
                ) :: Nil
              ) if cases.nonEmpty && cases.forall {
                case CaseDef(Unapply(TypeApply(Select(Ident("Tuple2"), "unapply"), _), _, _), _, _) =>
                  true
                case _ =>
                  false
              } && x.tpe.derivesFrom(mapSymbol) =>
            error(tree.pos, "You can use `foreachEntry` instead of `foreach` if Scala 2.13+")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
