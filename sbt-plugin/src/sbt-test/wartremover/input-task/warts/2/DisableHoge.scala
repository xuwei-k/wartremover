package example

import org.wartremover._

object DisableHoge extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    import u.universe._
    new u.Traverser {
      override def traverse(tree: Tree): Unit = {
        tree match {
          case d @ DefDef(_, TermName("hoge"), _, _, _, _) =>
            error(u)(tree.pos, "disable hoge")
          case _ =>
        }
        super.traverse(tree)
      }
    }
  }
}
