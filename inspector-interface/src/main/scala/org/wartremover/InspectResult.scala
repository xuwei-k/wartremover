package org.wartremover

final case class InspectResult(
  errors: Seq[Diagnostic],
  warnings: Seq[Diagnostic],
)

object InspectResult {
  val empty: InspectResult = InspectResult(Nil, Nil)
}
