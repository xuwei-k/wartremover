package org.wartremover

import dotty.tools.dotc.ast.tpd
import scala.annotation.nowarn
import scala.quoted.Quotes
import scala.quoted.Type
import java.lang.SuppressWarnings

class WartUniverse(val quotes: Quotes, traverser: WartTraverser, onlyWarning: Boolean, logLevel: LogLevel) { self =>
  abstract class Traverser extends quotes.reflect.TreeTraverser {
    final implicit val q: self.quotes.type = self.quotes
    import q.reflect.*

    protected[this] final def sourceCodeContains(t: Tree, src: String): Boolean = {
      // avoid StringIndexOutOfBoundsException
      // Don't use `def sourceCode: Option[String]`
      // https://github.com/lampepfl/dotty/blob/58b59a5f88508bb4b3/compiler/src/scala/quoted/runtime/impl/QuotesImpl.scala#L2791-L2793
      t.pos.sourceFile.content.exists { content =>
        val sliced = content.slice(t.pos.start, t.pos.end)
        sliced.contains(src)
      }
    }

    def getSyntheticPartialFunction(tree: Tree): Option[ClassDef] = {
      PartialFunction.condOpt(tree) {
        case c: ClassDef
            if c.symbol.flags.is(Flags.Synthetic) && c.parents.collect { case t: TypeTree =>
              t.tpe
            }.exists(
              _ <:< TypeRepr.of[PartialFunction[Nothing, Any]]
            ) =>
          c
      }
    }

    def isPrimitive(t: TypeRepr): Boolean = {
      t <:< TypeRepr.of[Boolean] ||
      t <:< TypeRepr.of[Byte] ||
      t <:< TypeRepr.of[Short] ||
      t <:< TypeRepr.of[Char] ||
      t <:< TypeRepr.of[Int] ||
      t <:< TypeRepr.of[Long] ||
      t <:< TypeRepr.of[Float] ||
      t <:< TypeRepr.of[Double]
    }

    def hasWartAnnotation(t: Tree): Boolean = {
      hasWartAnnotationSymbol(t.symbol) || Option(t.symbol.maybeOwner)
        .filterNot(_.isNoSymbol)
        .filter(s => s.isClassDef || s.isValDef || s.isDefDef)
        .exists(hasWartAnnotationSymbol)
    }
    private[this] val SuppressWarningsSymbol = TypeTree.of[SuppressWarnings].symbol
    private[this] def hasWartAnnotationSymbol(s: Symbol): Boolean = {
      val args: Set[String] = s
        .getAnnotation(SuppressWarningsSymbol)
        .collect {
          case a1 if a1.isExpr =>
            PartialFunction
              .condOpt(a1.asExpr) { case '{ new SuppressWarnings($a2: Array[String]) } =>
                PartialFunction
                  .condOpt(a2.asTerm) { case Apply(Apply(_, Typed(e, _) :: Nil), _) =>
                    e.asExprOf[Seq[String]].value
                  }
                  .flatten
              }
              .flatten
        }
        .flatten
        .toList
        .flatten
        .toSet

      args.contains(traverser.fullName) || args("org.wartremover.warts.All")
    }
    @nowarn("msg=dotty.tools.dotc.ast.tpd")
    override def foldOverTree(x: Unit, tree: Tree)(owner: Symbol): Unit = {
      try {
        tree match {
          case _: tpd.Template =>
          case _: tpd.Typed =>
          case _ =>
            super.foldOverTree(x, tree)(owner)
        }
      } catch {
        case e: MatchError =>
          if (logLevel != LogLevel.Disable) {
            warning(tree.pos, s"MatchError ${tree.getClass} ${owner.getClass}", traverser.simpleName)
          }
      }
    }
  }

  private[this] def withPrefix(name: String) = s"[wartremover:${name}] "

  final def warning(pos: quotes.reflect.Position, message: String, wartName: String): Unit = {
    quotes.reflect.report.warning(withPrefix(wartName) + message, pos)
  }
  final def error(pos: quotes.reflect.Position, message: String, wartName: String): Unit = {
    if (onlyWarning) {
      quotes.reflect.report.warning(withPrefix(wartName) + message, pos)
    } else {
      quotes.reflect.report.error(withPrefix(wartName) + message, pos)
    }
  }
}
