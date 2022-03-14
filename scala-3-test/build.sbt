addCompilerPlugin("org.wartremover" % "wartremover" % "2.4.19-SNAPSHOT" cross CrossVersion.full)

scalacOptions += "-P:wartremover:traverser:org.wartremover.warts.AnyVal"

scalaVersion := "3.2.0-RC1-bin-20220308-29073f1-NIGHTLY"
