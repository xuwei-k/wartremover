package org.wartremover
package warts

object DoobieLogHandler extends WartTraverser {
  private[wartremover] def message: String = "Don't use LogHandler.nop"
  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t: Select
              if t.name.contains("$default$") && (
                t.signature.exists(_.resultSig == "doobie.util.log$.LogHandler") ||
                  t.tpe.baseClasses.exists(_.fullName == "doobie.util.log$.LogHandler")
              ) =>
            error(tree.pos, message)
          case _ =>
        }
        super.traverseTree(tree)(owner)
      }
    }
  }
}
