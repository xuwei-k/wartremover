package org.wartremover

import scala.annotation.MacroAnnotation
import scala.annotation.experimental
import scala.quoted.Quotes

@experimental
final class WartremoverAnnotation(
  wart: WartTraverser,
  onlyWarning: Boolean = false,
  logLevel: LogLevel = LogLevel.Disable
) extends MacroAnnotation {

  def transform(using
    q: Quotes
  )(
    definition: q.reflect.Definition,
    companion: Option[q.reflect.Definition]
  ): List[q.reflect.Definition] = {
    val universe = WartUniverse[q.type](
      onlyWarning = onlyWarning,
      logLevel = logLevel,
      quotes = q
    )
    val traverser = wart(universe)
    traverser.traverseTree(definition)(q.reflect.Symbol.spliceOwner)
    definition :: Nil
  }

}
