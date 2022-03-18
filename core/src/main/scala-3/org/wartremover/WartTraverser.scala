package org.wartremover

abstract class WartTraverser { self =>
  def apply(u: WartUniverse): u.Traverser

  def compose(o: WartTraverser): WartTraverser = new WartTraverser {
    override def apply(u: WartUniverse): u.Traverser = {
      import u.quotes.reflect.*
      new u.Traverser {
        override def traverseTree(tree: Tree)(owner: Symbol): Unit = {
          self.apply(u).traverseTree(tree)(owner)
          o.apply(u).traverseTree(tree)(owner)
        }
      }
    }
  }
}

object WartTraverser {
  def sumList(u: WartUniverse)(l: List[WartTraverser]): u.Traverser =
    l.reduceRight(_ compose _)(u)
}
