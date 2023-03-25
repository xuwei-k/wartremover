def Scala212 = "2.12.17"

val commonSettings = Def.settings(
  wartremoverCrossVersion := CrossVersion.binary,
  scalaVersion := Scala212,
  crossScalaVersions := Seq(
   Scala212,
   "2.13.10",
   "3.2.2",
 )
)

commonSettings

val a1 = project.settings(commonSettings)
val a2 = project.settings(commonSettings)
