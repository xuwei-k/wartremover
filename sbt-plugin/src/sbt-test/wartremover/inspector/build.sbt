val root = project.in(file("."))

Compile / wartremoverInspect / wartremoverWarnings ++= Warts.all

scalaVersion := "3.1.2"

ThisBuild / wartremoverCrossVersion := CrossVersion.binary
