package org.brianmckenna.wartremover
package warts

// https://github.com/scala/scala/blob/v2.11.5/src/compiler/scala/tools/nsc/typechecker/RefChecks.scala#L1130-L1145
object InfiniteLoop extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._

    new u.Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case valOrDef0: ValOrDefDef =>
            val valOrDef = valOrDef0.asInstanceOf[scala.reflect.internal.Trees#ValOrDefDef]
            def callsSelf = valOrDef0.rhs match {
              case t@(Ident(_) | Select(This(_), _)) =>
                t.asInstanceOf[scala.reflect.internal.Trees#Tree].hasSymbolWhich(_.accessedOrSelf == valOrDef.symbol)
              case _ => false
            }
            if(
              !valOrDef.isErroneous
                && !valOrDef.symbol.isValueParameter
                && valOrDef.symbol.paramss.isEmpty
                && callsSelf
            ){
              u.error(valOrDef0.rhs.pos, s"${valOrDef.symbol.fullLocationString} does nothing other than call itself recursively")
            }else {
              super.traverse(tree)
            }
          case _ =>
            super.traverse(tree)
        }
      }
    }
  }
}
