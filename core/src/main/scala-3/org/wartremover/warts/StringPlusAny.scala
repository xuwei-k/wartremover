package org.wartremover
package warts

import scala.quoted.Expr

object StringPlusAny extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser {
      import q.reflect.*
      object PrimitivePlusString {
        def unapply[A](t: Expr[A]): Boolean = t match {
          case '{ ($x1: Byte) + ($x2: String) } => true
          case '{ ($x1: Short) + ($x2: String) } => true
          case '{ ($x1: Char) + ($x2: String) } => true
          case '{ ($x1: Int) + ($x2: String) } => true
          case '{ ($x1: Long) + ($x2: String) } => true
          case '{ ($x1: Float) + ($x2: String) } => true
          case '{ ($x1: Double) + ($x2: String) } => true
          case _ => false
        }
      }
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case t if hasWartAnnotation(t) =>
          case Apply(Select(lhs, "+"), List(rhs))
              if lhs.tpe <:< TypeRepr.of[String] && !(rhs.tpe <:< TypeRepr.of[String]) =>
            error(u)(tree.pos, "Implicit conversion to string is disabled")
          case t if t.isExpr =>
            t.asExpr match {
              case '{ new Predef.any2stringadd($x) } =>
                error(u)(tree.pos, "Implicit conversion to string is disabled")
              case PrimitivePlusString() =>
                error(u)(tree.pos, "Implicit conversion to string is disabled")
              case '{ ($x1: String) + ($x2: String) } =>
              case _ =>
                super.traverseTree(tree)(owner)
            }
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
