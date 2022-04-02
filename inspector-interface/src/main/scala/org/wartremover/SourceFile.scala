package org.wartremover

final case class SourceFile(
  name: String,
  path: String,
  content: Option[String],
)
