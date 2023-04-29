package org.wartremover
package warts

import scala.quoted.Quotes
import scala.quoted.Type
import scala.quoted.runtime.impl.QuotesImpl
import dotty.tools.dotc.core.Types.OrType as DottyOrType

object UnsoundEqual extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if hasWartAnnotation(tree) =>
          case Apply(Select(x1, "=="), x2 :: Nil) => if !(x1.tpe.widen =:= x2.tpe.widen) =>
            val lub = {
              implicit val ctx = q.asInstanceOf[QuotesImpl].ctx
              OrType(x1.tpe, x2.tpe).asInstanceOf[DottyOrType].join.asInstanceOf[TypeRepr]
            }
            if ((lub =:= TypeRepr.of[AnyRef]) || (lub =:= TypeRepr.of[Any])) {
              val left = x1.tpe.widen.show
              val right = x2.tpe.widen.show
              error(tree.pos, s"unsound equal `${lub.show}`. `${left} == ${right}`")
            }
            super.traverseTree(tree)(owner)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
