package org.wartremover
package warts

import java.util.concurrent.atomic.LongAdder

object RedundantConversions extends WartTraverser {
  private val values = collection.mutable.Map.empty[String, LongAdder]

  private var sum: Int = 0

  private def f(s: String): Unit = {
    sum += 1
    values.getOrElseUpdate(s, new LongAdder).increment()
    if (sum % 1000 == 0) {
      values.view.mapValues(_.sum()).toSeq.sortBy(_._2).foreach(println)
    }
  }

  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case _: Ident => f("Ident")
          case _: Select => f("Select")
          case _: This => f("This")
          case _: Super => f("Super")
          case _: Apply => f("Apply")
          case _: TypeApply => f("TypeApply")
          case _: Literal => f("Literal")
          case _: New => f("New")
          case _: Typed => f("Typed")
          case _: TypedOrTest => f("TypedOrTest")
          case _: NamedArg => f("NamedArg")
          case _: Assign => f("Assign")
          case _: Block => f("Block")
          case _: If => f("If")
          case _: While => f("While")
          case _: Closure => f("Closure")
          case _: Match => f("Match")
          case _: Return => f("Return")
          case _: Try => f("Try")
          case _: Repeated => f("Repeated")
          case _: Inlined => f("Inlined")
          case _: ValDef => f("ValDef")
          case _: DefDef => f("DefDef")
          case _: TypeDef => f("TypeDef")
          case _: ClassDef => f("ClassDef")
          case _: Import => f("Import")
          case _: Export => f("Export")
          case _: PackageClause => f("PackageClause")
          case _: Inferred => f("Inferred")
          case _: TypeIdent => f("TypeIdent")
          case _: TypeSelect => f("TypeSelect")
          case _: TypeProjection => f("TypeProjection")
          case _: Singleton => f("Singleton")
          case _: Refined => f("Refined")
          case _: Applied => f("Applied")
          case _: ByName => f("ByName")
          case _: Annotated => f("Annotated")
          case _: LambdaTypeTree => f("LambdaTypeTree")
          case _: TypeBind => f("TypeBind")
          case _: TypeBlock => f("TypeBlock")
          case _: MatchTypeTree => f("MatchTypeTree")
          case _: WildcardTypeTree => f("WildcardTypeTree")
          case _: TypeBoundsTree => f("TypeBoundsTree")
          case _: CaseDef => f("CaseDef")
          case _: TypeCaseDef => f("TypeCaseDef")
          case _: Bind => f("Bind")
          case _: Unapply => f("Unapply")
          case _: Alternatives => f("Alternatives")
          case _: SummonFrom => f("SummonFrom")
        }
        super.traverseTree(tree)(owner)
      }
    }
  }
}
