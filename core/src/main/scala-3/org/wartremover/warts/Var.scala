package org.wartremover
package warts

import org.wartremover.WartTraverser

object Var extends WartTraverser {

  private[this] val xmlClasses: List[Class[?]] =
    try {
      List(
        "scala.xml.MetaData",
        "scala.xml.NamespaceBinding"
      ).map(c => Class.forName(c))
    } catch {
      case _: ClassNotFoundException =>
        Nil
    }

  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*

      private def notXmlTypes(t: ValDef): Boolean = {
        !xmlClasses.map(TypeRepr.typeConstructorOf).exists(t.tpt.tpe =:= _)
      }

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case t: ValDef if t.symbol.flags.is(Flags.Mutable) && !t.symbol.flags.is(Flags.Synthetic) && notXmlTypes(t) =>
            error(u)(t.pos, "var is disabled")
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
