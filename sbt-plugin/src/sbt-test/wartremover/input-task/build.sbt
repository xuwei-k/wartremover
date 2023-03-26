val commonSettings = Def.settings(
  wartremoverCrossVersion := CrossVersion.binary,
  crossScalaVersions := Seq(
   "2.12.17",
   "2.13.10",
   "3.2.2",
 )
)

commonSettings

val a1 = project.settings(commonSettings)
val a2 = project.settings(commonSettings)
