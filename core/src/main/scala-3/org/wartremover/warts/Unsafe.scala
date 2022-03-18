package org.wartremover
package warts

object Unsafe extends WartTraverser {

  def apply(u: WartUniverse): u.Traverser =
    WartTraverser.sumList(u)(SafeWarts.safeTraversers)
}
