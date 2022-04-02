package wartremover

import org.wartremover.InspectParam
import org.wartremover.InspectResult
import java.nio.file.Path
import java.net.URL
import sbt.*
import sbt.Keys.*
import sbt.internal.librarymanagement.IvySbt
import sbt.librarymanagement.UnresolvedWarningConfiguration
import sbt.librarymanagement.UpdateConfiguration
import sbt.librarymanagement.ivy.IvyDependencyResolution
import sjsonnew.JsonFormat
import sjsonnew.JsonReader
import sjsonnew.support.scalajson.unsafe.CompactPrinter

object WartRemover extends sbt.AutoPlugin {
  override def trigger = allRequirements
  object autoImport {
    val wartremoverFailIfWartLoadError = settingKey[Boolean]("")
    val wartremoverInspect = taskKey[Unit]("run wartremover by TASTy inspector")
    val wartremoverInspectFailOnErrors = settingKey[Boolean]("")
    val wartremoverInspectScalaVersion = settingKey[String]("scala version for wartremoverInspect task")
    val wartremoverErrors = settingKey[Seq[Wart]]("List of Warts that will be reported as compilation errors.")
    val wartremoverWarnings = settingKey[Seq[Wart]]("List of Warts that will be reported as compilation warnings.")
    val wartremoverExcluded = taskKey[Seq[File]]("List of files to be excluded from all checks.")
    val wartremoverClasspaths = taskKey[Seq[String]]("List of classpaths for custom Warts")
    val wartremoverCrossVersion = settingKey[CrossVersion]("CrossVersion setting for wartremover")
    val wartremoverDependencies = settingKey[Seq[ModuleID]]("List of dependencies for custom Warts")
    val wartremoverPluginJarsDir = settingKey[Option[File]]("workaround for https://github.com/sbt/sbt/issues/6027")
    val Wart = wartremover.Wart
    val Warts = wartremover.Warts
  }
  import autoImport._

  override def globalSettings = Seq(
    wartremoverInspectScalaVersion := {
      // need NIGHTLY version because there are some bugs in old tasty-inspector.
      "3.1.3-RC1-bin-20220330-ee6733a-NIGHTLY",
    },
    wartremoverCrossVersion := CrossVersion.full,
    wartremoverDependencies := Nil,
    wartremoverErrors := Nil,
    wartremoverWarnings := Nil,
    wartremoverExcluded := Nil,
    wartremoverClasspaths := Nil
  )

  private[this] lazy val generateProject = {
    val id = "wartremover-inspector-project"
    Project(id = id, base = file("target") / id).settings(
      run / fork := true,
      fork := true,
      autoScalaLibrary := false,
      scalaVersion := wartremoverInspectScalaVersion.value,
      libraryDependencies := {
        if (scalaBinaryVersion.value == "3") {
          Seq(
            "org.scala-lang" % "scala3-tasty-inspector_3" % wartremoverInspectScalaVersion.value,
            "org.wartremover" % "wartremover-inspector_3" % Wart.PluginVersion,
          )
        } else {
          Nil
        }
      }
    )
  }

  // avoid extraProjects https://github.com/sbt/sbt/issues/4947
  override def derivedProjects(proj: ProjectDefinition[?]): Seq[Project] = {
    proj.projectOrigin match {
      case ProjectOrigin.DerivedProject =>
        Nil
      case _ =>
        Seq(generateProject)
    }
  }

  private[this] def copyToCompilerPluginJarsDir(
    src: File,
    jarDir: Option[File],
    base: File,
    log: Logger
  ): Option[Path] = {
    jarDir match {
      case Some(compilerPluginsDir) =>
        if (src.isFile) {
          val jarName = src.getName
          val targetJar = compilerPluginsDir / jarName
          if (!targetJar.isFile) {
            IO.copyFile(
              sourceFile = src,
              targetFile = targetJar
            )
            log.debug(s"copy from $src to $targetJar")
          } else {
            log.debug(s"file $targetJar already exists")
          }
          Some(base.toPath.relativize(targetJar.toPath))
        } else {
          if (!src.isDirectory) {
            log.debug(s"neither file nor directory!? $src")
          }
          Some(base.toPath.relativize(src.toPath))
        }
      case None =>
        log.debug("jarDir is None")
        None
    }
  }

  def scalacOptionSetting(k: TaskKey[Seq[String]]): Def.SettingsDefinition = {
    k := {
      val prefix = "-Xplugin:"
      k.value.map { opt =>
        if (opt startsWith prefix) {
          val originalPluginFile = file(opt.drop(prefix.length))
          copyToCompilerPluginJarsDir(
            src = originalPluginFile,
            jarDir = wartremoverPluginJarsDir.value,
            base = (LocalRootProject / baseDirectory).value,
            log = streams.value.log
          ).map {
            prefix + _
          }.getOrElse(opt)
        } else {
          opt
        }
      }.distinct
    }
  }

  def dependsOnLocalProjectWarts(p: Reference, configuration: Configuration = Compile): Def.SettingsDefinition = {
    wartremoverClasspaths ++= {
      (p / configuration / fullClasspath).value.map(_.data).map { f =>
        copyToCompilerPluginJarsDir(
          src = f,
          jarDir = wartremoverPluginJarsDir.value,
          base = (LocalRootProject / baseDirectory).value,
          log = streams.value.log
        ).map("file:" + _).getOrElse(f.toURI.toString)
      }
    }
  }

  private[this] implicit val inspectParamFormat: JsonFormat[InspectParam] = {
    import sjsonnew.BasicJsonProtocol.*
    caseClass6(InspectParam, InspectParam.unapply)(
      "tastyFiles",
      "dependenciesClasspath",
      "wartClasspath",
      "errorWarts",
      "warningWarts",
      "failIfWartLoadError",
    )
  }

  private[this] implicit val inspectResultFormat: JsonFormat[InspectResult] = {
    import sjsonnew.BasicJsonProtocol.*

    implicit val sourceFileInstance: JsonFormat[org.wartremover.SourceFile] =
      caseClass3(org.wartremover.SourceFile, org.wartremover.SourceFile.unapply)(
        "name",
        "path",
        "content",
      )

    implicit val positionInstance: JsonFormat[org.wartremover.Position] =
      caseClass8(org.wartremover.Position, org.wartremover.Position.unapply)(
        "start",
        "startLine",
        "startColumn",
        "end",
        "endLine",
        "endColumn",
        "sourceFile",
        "sourceCode",
      )

    implicit val diagnosticInstance: JsonFormat[org.wartremover.Diagnostic] =
      caseClass2(org.wartremover.Diagnostic, org.wartremover.Diagnostic.unapply)(
        "message",
        "position",
      )

    caseClass2(InspectResult.apply, InspectResult.unapply)(
      "errors",
      "warnings",
    )
  }

  private[this] implicit class JsonOps[A](private val self: A) extends AnyVal {
    def toJsonString(implicit w: sjsonnew.JsonWriter[A]): String = {
      val builder = new sjsonnew.Builder(sjsonnew.support.scalajson.unsafe.Converter.facade)
      w.write(self, builder)
      CompactPrinter.apply(
        builder.result.getOrElse(sys.error("invalid json"))
      )
    }
  }

  private[this] implicit class JsonStringOps(private val string: String) extends AnyVal {
    def decodeFromJsonString[A](implicit r: sjsonnew.JsonReader[A]): A = {
      val json = sjsonnew.support.scalajson.unsafe.Parser.parseUnsafe(string)
      println(json)
      val unbuilder = new sjsonnew.Unbuilder(sjsonnew.support.scalajson.unsafe.Converter.facade)
      r.read(Some(json), unbuilder)
    }
  }

  override lazy val projectSettings: Seq[Def.Setting[_]] = Def.settings(
    libraryDependencies ++= {
      Seq(
        compilerPlugin(
          "org.wartremover" %% "wartremover" % Wart.PluginVersion cross wartremoverCrossVersion.value
        )
      )
    },
    Seq(Compile, Test).flatMap { x =>
      Seq(
        x / wartremoverFailIfWartLoadError := false,
        x / wartremoverInspectFailOnErrors := true,
        x / wartremoverInspect := {
          val log = streams.value.log
          val s = state.value
          val extracted = Project.extract(s)
          val myProject = thisProjectRef.value
          val thisTaskName = s"${myProject.project}/${x.name}/${wartremoverInspect.key.label}"
          def skipLog(reason: String): Unit = log.info(s"skip ${thisTaskName} because ${reason}")
          if (scalaBinaryVersion.value == "3") {
            val errorWartNames = (x / wartremoverInspect / wartremoverErrors).value
            val warningWartNames = (x / wartremoverInspect / wartremoverWarnings).value
            if (errorWartNames.isEmpty && warningWartNames.isEmpty) {
              skipLog("warts is empty")
            } else {
              val tastys = extracted.runTask(myProject / x / tastyFiles, s)._2
              if (tastys.isEmpty) {
                skipLog(s"${tastyFiles.key.label} is empty")
              } else {
                import scala.language.reflectiveCalls
                val loader = extracted.runTask(generateProject / Test / testLoader, s)._2
                val clazz = loader.loadClass("org.wartremover.WartRemoverInspector")
                val instance = clazz
                  .getConstructor()
                  .newInstance()
                  .asInstanceOf[
                    {
                      def run(json: String): String
                    }
                  ]

                val dependenciesClasspath = extracted.runTask(myProject / x / fullClasspath, s)._2
                log.info(
                  s"running ${thisTaskName}. errorWarts = ${errorWartNames}, warningWarts = ${warningWartNames}"
                )
                val param = org.wartremover.InspectParam(
                  tastyFiles = tastys.map(_.getAbsolutePath).toList,
                  dependenciesClasspath = dependenciesClasspath.map(_.data.getAbsolutePath).toList,
                  wartClasspath = {
                    extracted
                      .runTask(myProject / x / wartremoverClasspaths, s)
                      ._2
                      .map {
                        case a if a.startsWith("file:") =>
                          file(a.drop("file:".length)).getCanonicalFile.toURI.toURL
                        case a =>
                          new URL(a)
                      }
                      .map(_.toString) // TODO
                  }.toList,
                  errorWarts = errorWartNames.map(_.clazz).toList,
                  warningWarts = warningWartNames.map(_.clazz).toList,
                  failIfWartLoadError = (x / wartremoverFailIfWartLoadError).value,
                )
                val result = instance.run(param.toJsonString).decodeFromJsonString[InspectResult]
                if (result.errors.nonEmpty && (x / wartremoverInspectFailOnErrors).value) {
                  sys.error(s"[${myProject.project}] wart error found")
                } else {
                  log.info(
                    s"finished ${thisTaskName}"
                  )
                }
              }
            }
          } else {
            skipLog(s"scalaVersion is ${scalaVersion.value}. not Scala 3")
          }
        }
      )
    },
    scalacOptionSetting(scalacOptions),
    scalacOptionSetting(Compile / scalacOptions),
    scalacOptionSetting(Test / scalacOptions),
    scalacOptions ++= {
      // use relative path
      // https://github.com/sbt/sbt/issues/6027
      wartremoverExcluded.value.distinct.map { c =>
        val base = (LocalRootProject / baseDirectory).value
        val x = base.toPath.relativize(c.toPath)
        s"-P:wartremover:excluded:$x"
      }
    },
    wartremoverPluginJarsDir := {
      if (VersionNumber(sbtVersion.value).matchesSemVer(SemanticSelector(">=1.4.0"))) {
        Some((LocalRootProject / target).value / "compiler_plugins")
      } else {
        None
      }
    },
    inScope(Scope.ThisScope)(
      Seq(
        wartremoverClasspaths ++= {
          val ivy = ivySbt.value
          val s = streams.value
          wartremoverDependencies.value.map { m =>
            val moduleId = CrossVersion(
              cross = m.crossVersion,
              fullVersion = scalaVersion.value,
              binaryVersion = scalaBinaryVersion.value
            ) match {
              case Some(f) =>
                m.withName(f(Project.normalizeModuleID(m.name)))
              case None =>
                m
            }
            val a = getArtifact(moduleId, ivy, s)
            copyToCompilerPluginJarsDir(
              src = a,
              jarDir = wartremoverPluginJarsDir.value,
              base = (LocalRootProject / baseDirectory).value,
              log = streams.value.log
            ).map("file:" + _).getOrElse(a.toURI.toString)
          }
        },
        derive(
          scalacOptions ++= {
            wartremoverErrors.value.distinct map (w => s"-P:wartremover:traverser:${w.clazz}")
          }
        ),
        derive(
          scalacOptions ++= {
            wartremoverWarnings.value.distinct filterNot (wartremoverErrors.value contains _) map (w =>
              s"-P:wartremover:only-warn-traverser:${w.clazz}"
            )
          }
        ),
        derive(
          scalacOptions ++= {
            wartremoverClasspaths.value.distinct map (cp => s"-P:wartremover:cp:$cp")
          }
        )
      )
    )
  )

  // Workaround for https://github.com/wartremover/wartremover/issues/123
  private[wartremover] def derive[T](s: Setting[T]): Setting[T] = {
    try {
      Def derive s
    } catch {
      case _: LinkageError =>
        import scala.language.reflectiveCalls
        Def
          .asInstanceOf[{
              def derive[T](
                setting: Setting[T],
                allowDynamic: Boolean,
                filter: Scope => Boolean,
                trigger: AttributeKey[_] => Boolean,
                default: Boolean
              ): Setting[T]
            }
          ]
          .derive(s, false, _ => true, _ => true, false)
    }
  }

  /**
   * [[https://github.com/lightbend/mima/blob/723bd0046c0c6a4f52c91ddc752d08dce3b7ba37/sbtplugin/src/main/scala/com/typesafe/tools/mima/plugin/SbtMima.scala#L79-L100]]
   * @note avoid coursier for sbt 1.2.x compatibility
   */
  private[this] def getArtifact(m: ModuleID, ivy: IvySbt, s: TaskStreams): File = {
    val depRes = IvyDependencyResolution(ivy.configuration)
    val module = depRes.wrapDependencyInModule(m)
    val uc = UpdateConfiguration().withLogging(UpdateLogging.DownloadOnly)
    val uwc = UnresolvedWarningConfiguration()
    val report = depRes.update(module, uc, uwc, s.log).left.map(_.resolveException).toTry.get
    val jars = (for {
      config <- report.configurations.iterator
      module <- config.modules
      (artifact, file) <- module.artifacts
      if artifact.name == m.name
      if artifact.classifier.isEmpty
    } yield file).toList.distinct
    jars match {
      case jar :: Nil =>
        jar
      case Nil =>
        sys.error(s"Could not resolve: $m $jars")
      case jar :: _ =>
        s.log.info(s"multiple jar found $jars")
        jar
    }
  }

}
