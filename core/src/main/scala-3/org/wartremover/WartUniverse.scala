package org.wartremover

import scala.quoted.Quotes
import scala.quoted.Type
import java.lang.SuppressWarnings

class WartUniverse(val quotes: Quotes, traverser: WartTraverser, onlyWarning: Boolean) { self =>
  abstract class Traverser extends quotes.reflect.TreeTraverser {
    private[this] def simpleName: String = traverser.getClass.getSimpleName.dropRight(1)
    private[this] def fullName: String = traverser.getClass.getName.dropRight(1)

    protected def messagePrefix = s"[wartremover:${simpleName}] "
    final implicit val q: self.quotes.type = self.quotes
    import q.reflect.*
    def hasWartAnnotation(t: Tree): Boolean = {
      val args: Set[String] = t.symbol.getAnnotation(TypeTree.of[SuppressWarnings].symbol).collect {
        case a1 if a1.isExpr =>
          PartialFunction.condOpt(a1.asExpr) {
            case '{ new SuppressWarnings($a2: Array[String]) } =>
              PartialFunction.condOpt(a2.asTerm) {
                case Apply(Apply(_, Typed(e, _) :: Nil), _) =>
                  e.asExprOf[Seq[String]].value
              }.flatten
          }.flatten
      }.flatten.toList.flatten.toSet

      args.contains(fullName) || args("org.wartremover.warts.All")
    }
    override protected def traverseTreeChildren(tree: Tree)(owner: Symbol): Unit = {
      try {
        super.traverseTreeChildren(tree)(owner)
      } catch {
        case e: MatchError =>
          warning(self)(tree.pos, s"MatchError ${tree.getClass} ${owner.getClass}")
      }
    }
    // TODO remove Universe param?
    protected final def warning(u: WartUniverse)(pos: Position, message: String): Unit = {
      report.warning(messagePrefix + message, pos)
    }
    // TODO remove Universe param?
    protected final def error(u: WartUniverse)(pos: Position, message: String): Unit = {
      if (onlyWarning) {
        report.warning(messagePrefix + message, pos)
      } else {
        report.error(messagePrefix + message, pos)
      }
    }
  }
}
