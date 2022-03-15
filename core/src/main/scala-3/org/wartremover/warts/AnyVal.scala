package org.wartremover
package warts

object AnyVal extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    implicit val q = u.quotes
    new u.ForbidInferenceTraverser[scala.AnyVal]
  }
}
