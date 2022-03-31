val root = project.in(file("."))

Compile / wartremoverInspect / wartremoverWarnings ++= Warts.all

scalaVersion := "3.1.1"

ThisBuild / wartremoverCrossVersion := CrossVersion.binary
