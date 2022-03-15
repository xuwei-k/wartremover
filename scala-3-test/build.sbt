addCompilerPlugin("org.wartremover" % "wartremover" % "2.4.19-SNAPSHOT" cross CrossVersion.full)

scalacOptions ++= {
  Seq(
    "Any",
    "NoooooootExistsWartName",
    "Var",
    "ListUnapply",
    "ArrayEquals",
    "Return",
    "ScalaApp",
    "AnyVal",
    "While",
  ).map("-P:wartremover:only-warn-traverser:org.wartremover.warts." + _)
}

scalaVersion := "3.1.1"
