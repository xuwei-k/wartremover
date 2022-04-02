package org.wartremover

final case class InspectParam(
  tastyFiles: Seq[String],
  dependenciesClasspath: Seq[String],
  wartClasspath: Seq[String],
  errorWarts: Seq[String],
  warningWarts: Seq[String],
  failIfWartLoadError: Boolean,
)
