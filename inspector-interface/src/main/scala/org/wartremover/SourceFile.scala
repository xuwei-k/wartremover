package org.wartremover

import java.nio.file.Path

final case class SourceFile(
  jpath: Option[Path],
  name: String,
  path: String,
  content: Option[String],
)
