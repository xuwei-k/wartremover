package org.wartremover

import scala.quoted.Quotes

abstract class WartTraverser{
  def apply(u: WartUniverse): u.Traverser
}

trait WartUniverse {
  val quotes: Quotes
  abstract class Traverser extends org.wartremover.Traverser(quotes)
}

trait Traverser(val q: Quotes) {
  protected final implicit def quotes: q.type = q
  def traverse(tree: q.reflect.Tree): Unit
}