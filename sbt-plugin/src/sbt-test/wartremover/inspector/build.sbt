val root = project.in(file("."))

Compile / wartremoverInspect / wartremoverWarnings ++= Warts.all

scalaVersion := "3.1.2"

ThisBuild / wartremoverCrossVersion := CrossVersion.binary

Compile / wartremoverInspectFile := baseDirectory.value / "warts-main.json"
Test / wartremoverInspectFile := baseDirectory.value / "warts-test.json"
