package org.wartremover

import dotty.tools.dotc.ast.tpd.InferredTypeTree
import scala.quoted.Quotes
import scala.quoted.Type
import scala.annotation.nowarn

abstract class WartTraverser{
  def apply(u: WartUniverse): u.Traverser
}

class WartUniverse(val quotes: Quotes, traverser: WartTraverser) { self =>
  class ForbidInferenceTraverser[A: Type] extends Traverser {
    import quotes.reflect.*
    private[this] val A = TypeRepr.of[A]
    @nowarn("msg=cannot be checked at runtime")
    override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
      tree match {
        case _ if hasWartAnnotation(tree) =>
        case a: Inferred if a.tpe =:= A && a.isInstanceOf[InferredTypeTree] =>
          val name = A.show.split('.').last // TODO more better way
          error(self)(tree.pos, s"Inferred type containing ${name}: ${name}")
        case _ =>
          super.traverseTree(tree)(owner)
      }
    }
  }

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
      report.error(messagePrefix + message, pos)
    }
  }
}
