package org.wartremover

final case class Diagnostic(
  message: String,
  position: Position
)
