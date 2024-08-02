package org.wartremover
package warts

import scala.quoted.Type
import scala.quoted.Quotes

object Discard {
  object Either
      extends Discard(
        [a <: AnyKind] =>
          (tpe: Type[a]) =>
            tpe match {
              case '[Either[?, ?]] =>
                true
              case _ =>
                false
          }
      )

  object Future
      extends Discard(
        [a <: AnyKind] =>
          (tpe: Type[a]) =>
            tpe match {
              case '[scala.concurrent.Future[?]] =>
                true
              case _ =>
                false
          }
      )

  object Try
      extends Discard(
        [a <: AnyKind] =>
          (tpe: Type[a]) =>
            tpe match {
              case '[scala.util.Try[?]] =>
                true
              case _ =>
                false
          }
      )
}

abstract class Discard(filter: Quotes ?=> ([a <: AnyKind] => Type[a] => Boolean)) extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*

      def msg[B](t: TypeRepr): String =
        s"discard `${t.dealias.show}`"

      private def createAccumulator[A](f: (A, Tree) => A): TreeAccumulator[A] =
        new TreeAccumulator[A] {
          override def foldTree(x: A, t: Tree)(owner: Symbol): A =
            foldOverTree(f(x, t), t)(owner)
        }

      private def collectItent(t: Tree, owner: Symbol): Set[String] = {
        val accumulator = createAccumulator[Set[String]] {
          case (x, i: Ident) =>
            x + i.name
          case (x, _) =>
            x
        }
        accumulator.foldTree(Set.empty, t)(owner)
      }

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _ if hasWartAnnotation(tree) =>
          case t: Block =>
            t.statements.collect {
              case x: Term if filter(x.tpe.asType) => x
            }.foreach { x =>
              error(x.pos, msg(x.tpe))
            }
            super.traverseTree(tree)(owner)
          case t: ClassDef =>
            t.body.collect {
              case x: Term if filter(x.tpe.asType) => x
            }.foreach { x =>
              error(x.pos, msg(x.tpe))
            }
            super.traverseTree(tree)(owner)
          case f: DefDef if f.symbol.isAnonymousFunction =>
            val params = f.termParamss.flatMap(_.params).filter(x => filter(x.tpt.tpe.asType))

            if (params.nonEmpty) {
              f.rhs.foreach { body =>
                val bodyNames: Set[String] = collectItent(body, owner)
                params.filterNot(p => bodyNames(p.name)).foreach { x =>
                  error(x.pos, msg(x.tpt.tpe))
                }
              }
            }
            super.traverseTree(tree)(owner)
          case f: CaseDef =>
            f.pattern match {
              case Bind(x, w: Wildcard) if filter(w.tpe.asType) =>
                val namesSet: Set[String] =
                  collectItent(f.rhs, owner) ++ f.guard.map(x => collectItent(x, owner)).toSeq.flatten

                if (namesSet(x)) {
                  // ok
                } else {
                  error(f.pattern.pos, msg(w.tpe))
                }
              case _ =>
                val patternAccumulator = createAccumulator[Map[String, Ident]] {
                  case (x, i: Ident) if filter(i.tpe.asType) =>
                    x + (i.name -> i)
                  case (x, _) =>
                    x
                }
                val patternNames = patternAccumulator.foldTree(Map.empty, f.pattern)(owner)

                if (patternNames.nonEmpty) {
                  val namesSet: Set[String] =
                    collectItent(f.rhs, owner) ++ f.guard.map(x => collectItent(x, owner)).toSeq.flatten

                  patternNames.foreach { case (k, v) =>
                    if (namesSet(k)) {
                      // ok
                    } else {
                      error(f.pattern.pos, msg(v.tpe))
                    }
                  }
                }
            }

            super.traverseTree(tree)(owner)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
