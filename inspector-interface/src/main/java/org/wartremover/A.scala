package org.wartremover

import java.nio.file.Path

final case class InspectParam(
  tastyFiles: Seq[String],
  dependenciesClasspath: Seq[String],
  wartClasspath: Seq[String],
  errorWarts: Seq[String],
  warningWarts: Seq[String],
  failIfWartLoadError: Boolean,
)

final case class InspectResult(
  errors: Seq[Diagnostic],
  warnings: Seq[Diagnostic],
)

final case class Diagnostic(
  message: String,
  position: Position
)

final case class Position(
  start: Int,
  startLine: Int,
  startColumn: Int,
  end: Int,
  endLine: Int,
  endColumn: Int,
  sourceFile: SourceFile,
  sourceCode: Option[String],
)

final case class SourceFile(
  jpath: Option[Path],
  name: String,
  path: String,
  content: Option[String],
)
