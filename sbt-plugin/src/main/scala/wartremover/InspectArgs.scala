package wartremover

import wartremover.InspectArg.Type

private[wartremover] case class InspectArgs(
  sources: Seq[String],
  warts: Seq[Wart]
)

private[wartremover] object InspectArgs {
  val empty: InspectArgs = InspectArgs(Nil, Nil)
  def from(values: Seq[(InspectArg, Type)]): Map[Type, InspectArgs] = {
    val warnSources = List.newBuilder[String]
    val warnWarts = List.newBuilder[Wart]
    val errorSources = List.newBuilder[String]
    val errorWarts = List.newBuilder[Wart]
    values.foreach {
      case (x: InspectArg.FromSource, tpe) =>
        tpe match {
          case Type.Warn =>
            warnSources ++= x.getSourceContents
          case Type.Err =>
            errorSources ++= x.getSourceContents
        }
      case (x: InspectArg.WartName, tpe) =>
        tpe match {
          case Type.Warn =>
            warnWarts += Wart.custom(x.value)
          case Type.Err =>
            errorWarts += Wart.custom(x.value)
        }
    }
    Map(
      Type.Warn -> InspectArgs(
        sources = warnSources.result(),
        warts = warnWarts.result()
      ),
      Type.Err -> InspectArgs(
        sources = errorSources.result(),
        warts = errorWarts.result()
      ),
    )
  }
}
