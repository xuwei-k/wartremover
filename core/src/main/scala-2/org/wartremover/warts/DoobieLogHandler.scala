package org.wartremover.warts

import org.wartremover.WartTraverser
import org.wartremover.WartUniverse

object DoobieLogHandler extends WartTraverser {
  private[wartremover] def message: String = "Don't use LogHandler.nop"
  override def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    new u.Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case t: Select
              if t.name.toString.contains(
                "$default$"
              ) && (t.tpe.typeSymbol.fullName == "doobie.util.log.LogHandler") =>
            error(u)(tree.pos, message)
          case _ =>
        }
        super.traverse(tree)
      }
    }
  }
}
