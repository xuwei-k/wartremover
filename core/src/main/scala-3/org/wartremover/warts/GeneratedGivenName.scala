package org.wartremover
package warts

object GeneratedGivenName extends WartTraverser {
  override def apply(u: WartUniverse): u.Traverser = {
    new u.Traverser(this) {
      import q.reflect.*
      private def isGeneratedGiven(s: Symbol, name: String): Boolean = {
        println(s.methodMembers.filter(_.name.startsWith("given_")))
        val members = s.methodMember(s.name) ++ s.fieldMembers.filter(_.name == name)
        val res = members.filter(_.flags.is(Flags.Given)).exists(_.pos.exists(p => p.start == p.end))

        println((s,name, members, res, members.map(_.flags.show), members.map(_.pos.map(p => (p.start ,p.end)))))
        res
      }

      override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
        tree match {
          case i: Import =>
            if (
              i.selectors.exists {
                case s: SimpleSelector if s.name.startsWith("given_") =>
                  isGeneratedGiven(i.expr.symbol, s.name)
                case _ =>
                  false
              }
            ) {
              error(i.pos, "Don't use generated given name")
            }
            super.traverseTree(tree)(owner)
          case i: Ident
              if i.symbol.name.startsWith("given_") && i.symbol.flags.is(Flags.Given) && (i.pos.start != i.pos.end) =>
            println(
              (
                i.pos,
                i.pos.sourceCode,
                i.symbol.pos.map(_.sourceCode)
              )
            )
            error(i.pos, "Don't use generated given name")
            super.traverseTree(tree)(owner)
          case s @ Select(_, name @ s"given_${_}") if isGeneratedGiven(s.qualifier.symbol, name) =>
            error(s.pos, "Don't use generated given name")
            super.traverseTree(tree)(owner)
          case _ =>
            super.traverseTree(tree)(owner)
        }
      }
    }
  }
}
