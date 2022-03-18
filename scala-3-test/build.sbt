Compile / compile / wartremoverWarnings ++= Warts.all

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.0.1" % "provided"

scalacOptions += "-P:wartremover:loglevel:debug"

scalaVersion := "3.1.1"
