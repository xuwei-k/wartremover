import sbtrelease._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease.ReleaseStateTransformations._
import com.typesafe.sbt.pgp.PgpKeys._
import AssemblyKeys._

name := "wartremover"

organization := "org.brianmckenna"

scalaVersion := "2.10.4"

crossScalaVersions := Seq("2.10.4", "2.11.0-RC4")

crossVersion := CrossVersion.binary

releaseSettings

assemblySettings

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = st => {
    val extracted = Project.extract(st)
    val ref = extracted.get(thisProjectRef)
    extracted.runAggregated(publishSigned in Global in ref, st)
  }),
  setNextVersion,
  commitNextVersion,
  pushChanges
)

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.0-M7" cross CrossVersion.full)

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, 10)) =>
      libraryDependencies.value :+ ("org.scalamacros" %% "quasiquotes" % "2.0.0-M7")
    case _ =>
      libraryDependencies.value
  }
}

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "2.1.3" % "test"
)

scalacOptions in Test <++= packageBin in Compile map { pluginJar => Seq(
  "-Xplugin:" + pluginJar,
  "-P:wartremover:cp:" + pluginJar.toURI.toURL,
  "-P:wartremover:traverser:org.brianmckenna.wartremover.warts.Unsafe"
) }

publishMavenStyle := true

publishArtifact in Test := false

publishTo := sys.env.get("MAVEN_DIRECTORY").map{ dir =>
  Resolver.file("gh-pages", file(dir))(Patterns(true, Resolver.mavenStyleBasePattern))
}

licenses := Seq(
  "The Apache Software License, Version 2.0" ->
    url("http://www.apache.org/licenses/LICENSE-2.0.txt")
)

homepage := Some(url("https://github.com/puffnfresh/wartremover"))

pomExtra := (
  <scm>
    <url>git@github.com:puffnfresh/wartremover.git</url>
    <connection>scm:git:git@github.com:puffnfresh/wartremover.git</connection>
  </scm>
  <developers>
    <developer>
      <id>puffnfresh</id>
      <name>Brian McKenna</name>
      <url>http://brianmckenna.org/</url>
    </developer>
  </developers>
)
