package org.wartremover

import dotty.tools.dotc.ast.tpd
import scala.annotation.nowarn
import scala.quoted.Quotes
import scala.quoted.Type
import java.lang.SuppressWarnings

class WartUniverse(val quotes: Quotes, traverser: WartTraverser, onlyWarning: Boolean, logLevel: LogLevel) { self =>
  abstract class Traverser extends quotes.reflect.TreeTraverser {
    private[this] def simpleName: String = traverser.getClass.getSimpleName.dropRight(1)
    private[this] def fullName: String = traverser.getClass.getName.dropRight(1)

    protected def messagePrefix = s"[wartremover:${simpleName}] "
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

      args.contains(fullName) || args("org.wartremover.warts.All")
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
            warning(self)(tree.pos, s"MatchError ${tree.getClass} ${owner.getClass}")
          }
      }
    }
    protected final def warning(u: WartUniverse)(pos: u.quotes.reflect.Position, message: String): Unit = {
      u.quotes.reflect.report.warning(messagePrefix + message, pos)
    }
    protected final def error(u: WartUniverse)(pos: u.quotes.reflect.Position, message: String): Unit = {
      if (onlyWarning) {
        u.quotes.reflect.report.warning(messagePrefix + message, pos)
      } else {
        u.quotes.reflect.report.error(messagePrefix + message, pos)
      }
    }
  }
}
