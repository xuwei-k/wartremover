package org.wartremover

final case class InspectParam(
  tastyFiles: List[String],
  dependenciesClasspath: List[String],
  wartClasspath: List[String],
  errorWarts: List[String],
  warningWarts: List[String],
  failIfWartLoadError: Boolean,
)
