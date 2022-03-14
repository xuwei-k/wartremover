package org.wartremover

import scala.quoted.Quotes

abstract class WartTraverser{
  def apply(u: WartUniverse): u.Traverser
}

class WartUniverse(quotes: Quotes, traverser: WartTraverser) { self =>
  abstract class Traverser {
    private[this] def name = traverser.getClass.getSimpleName.dropRight(1)

    protected def messagePrefix = s"[wartremover:${name}] "
    final implicit val q: Quotes = self.quotes

    def traverse(tree: q.reflect.Tree): Unit

    protected final def warning(u: WartUniverse)(message: String): Unit = {
      q.reflect.report.warning(messagePrefix + message)
    }
    protected final def warning(u: WartUniverse)(message: String, pos: q.reflect.Position): Unit = {
      q.reflect.report.warning(messagePrefix + message, pos)
    }
    protected final def error(u: WartUniverse)(message: String): Unit = {
      q.reflect.report.error(messagePrefix + message)
    }
    protected final def error(u: WartUniverse)(message: String, pos: q.reflect.Position): Unit = {
      q.reflect.report.error(messagePrefix + message, pos)
    }
  }
}