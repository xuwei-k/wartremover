lazy val commonSettings = Def.settings(
  crossScalaVersions := Seq("2.11.12", "2.12.12", "2.13.3"),
  scalaVersion := "2.13.3",
  wartremoverWarnings ++= Warts.all,
  wartremoverWarnings += Wart.custom("org.wartremover.contrib.warts.OldTime"),
  wartremoverDependencies += {
    "org.wartremover" %% "wartremover-contrib" % "1.3.9" cross wartremoverCrossVersion.value
  },
  wartremoverWarnings += Wart.JavaConversions,
  Seq(Compile, Test).flatMap { s =>
    import sbt.internal.inc.Schema.AnalysisFile
    import sbt.internal.inc.Schema.APIsFile
    def load[A](fileName: String, c: Configuration, f: java.io.InputStream => A) =
      Def.task {
        sbt.io.Using.zipFile((c / compileAnalysisFile).value) { i =>
          val x = f(i.getInputStream(i.getEntry(fileName)))
          IO.write(baseDirectory.value / (fileName + ".txt"), x.toString)
          x
        }
      }
    Def.settings(
      s / TaskKey[AnalysisFile]("zincAnalysis") := {
        load("inc_compile.bin", s, AnalysisFile.parseFrom).value,
      },
      s / TaskKey[APIsFile]("zincApi") := {
        load("api_companions.bin", s, APIsFile.parseFrom).value
      }
    )
  }
)

scalaVersion := "2.12.12"

lazy val myWarts = project.in(file("my-warts")).settings(
  commonSettings,
  libraryDependencies ++= Seq(
    "org.wartremover" % "wartremover" % wartremover.Wart.PluginVersion cross CrossVersion.full
  )
)

lazy val a1 = project.settings(
  commonSettings,
  wartremoverErrors += Wart.custom("mywarts.Unimplemented"),
  wartremoverExcluded ++= (Compile / managedSourceDirectories).value,
  (Compile / sourceGenerators) += task {
    val dir = (Compile / sourceManaged).value
    val b = dir / "B.scala"
    IO.write(
      b,
      "object B { def b = ??? }"
    )
    Seq(b)
  },
  wartremover.WartRemover.dependsOnLocalProjectWarts(myWarts),
)
