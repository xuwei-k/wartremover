crossScalaVersions := Seq("2.11.12", "2.12.10", "2.12.11", "2.12.12", "2.13.0", "2.13.1", "2.13.2", "2.13.3")

scalaVersion := "2.13.3"

wartremoverWarnings ++= Warts.all

wartremoverWarnings += Wart.JavaConversions

commands += Command.command("changeBinary") {
  "set wartremoverCrossVersion := CrossVersion.binary" ::
  """set crossScalaVersions := Seq("2.11.12", "2.12.12", "2.13.3")""" :: // set latest versions
  _
}

Seq(Compile, Test).flatMap { s =>
  import sbt.internal.inc.Schema.AnalysisFile
  import sbt.internal.inc.Schema.APIsFile
  def load[A](fileName: String, c: Configuration, f: java.io.InputStream => A) =
    Def.task {
      sbt.io.Using.zipFile((c / compileAnalysisFile).value) { i =>
        val x = f(i.getInputStream(i.getEntry(fileName)))
        IO.write(file(fileName + ".txt"), x.toString)
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
