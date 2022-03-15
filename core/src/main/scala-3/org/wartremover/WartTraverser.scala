package org.wartremover

abstract class WartTraverser{
  def apply(u: WartUniverse): u.Traverser
}