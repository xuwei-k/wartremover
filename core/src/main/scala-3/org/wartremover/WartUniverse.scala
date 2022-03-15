package org.wartremover

import scala.quoted.Quotes
import scala.quoted.Type

class WartUniverse(val quotes: Quotes, traverser: WartTraverser, onlyWarning: Boolean) { self =>
  abstract class Traverser extends quotes.reflect.TreeTraverser {
    private[this] def name = traverser.getClass.getSimpleName.dropRight(1)

    protected def messagePrefix = s"[wartremover:${name}] "
    final implicit val q: self.quotes.type = self.quotes
    import q.reflect.*
    def hasWartAnnotation(t: Tree): Boolean = {
      t.symbol.getAnnotation(TypeTree.of[java.lang.SuppressWarnings].symbol) match {
        case Some(a) =>
          a // TODO
          true
        case _ =>
          false
      }
    }

    protected final def warning(u: WartUniverse)(pos: Position, message: String): Unit = {
      report.warning(messagePrefix + message, pos)
    }
    protected final def error(u: WartUniverse)(pos: Position, message: String): Unit = {
      if (onlyWarning) {
        report.warning(messagePrefix + message, pos)
      } else {
        report.error(messagePrefix + message, pos)
      }
    }
  }
}
