package org.wartremover

final case class InspectResult(
  errors: List[Diagnostic],
  warnings: List[Diagnostic],
)

object InspectResult {
  val empty: InspectResult = InspectResult(Nil, Nil)
}
