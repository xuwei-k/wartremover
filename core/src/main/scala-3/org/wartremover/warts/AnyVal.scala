package org.wartremover
package warts

import dotty.tools.dotc.ast.tpd.InferredTypeTree

object AnyVal extends WartTraverser {
  def apply(u: WartUniverse): u.Traverser = {
    implicit val q = u.quotes
    new u.ForbidInferenceTraverser[AnyVal]
  }
}
