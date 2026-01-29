package org.wartremover
package warts

object SortedMaxMin extends WartTraverser {
  private val sortMethodNames: Seq[String] = Seq(
    "sortBy",
    "sorted",
  )
  private val headOrLast: Seq[String] = Seq(
    "head",
    "last",
  )

  private object SortedHeadOrLast {
    def unapply(method: String): Option[String] =
      PartialFunction.condOpt(method) {
        case "head" =>
          "You can use min instead of sorted.head"
        case "last" =>
          "You can use max instead of sorted.last"
      }
  }

  private object SortByHeadOrLast {
    def unapply(method: String): Option[String] =
      PartialFunction.condOpt(method) {
        case "head" =>
          "You can use minBy instead of sortBy.head"
        case "last" =>
          "You can use maxBy instead of sortBy.last"
      }
  }

  def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _
              if sortMethodNames
                .forall(sourceCodeNotContains(tree, _)) || headOrLast.forall(sourceCodeNotContains(tree, _)) =>
          case t if hasWartAnnotation(t) =>
          case t @ Select(
                Apply(TypeApply(Select(seq, "sorted"), _ :: Nil), _ :: Nil),
                SortedHeadOrLast(message)
              ) if seq.tpe.baseClasses.exists(_.fullName == "scala.collection.Seq") =>
            error(selectNamePosition(t), message)
          case t @ Select(
                Apply(
                  Apply(TypeApply(Select(seq, "sortBy"), _ :: Nil), _ :: Nil),
                  _ :: Nil
                ),
                SortByHeadOrLast(message)
              ) if seq.tpe.baseClasses.exists(_.fullName == "scala.collection.Seq") =>
            error(selectNamePosition(t), message)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
