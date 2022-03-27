package org.wartremover

import coursier.util.Artifact
import scala.quoted.Quotes
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.reflect.ClassTag

object WartInspector {
  def timeString(): String = {
    java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(java.time.ZonedDateTime.now())
  }

  def jarPathFromType[A](implicit c: ClassTag[A]): String = {
    c.runtimeClass.getProtectionDomain.getCodeSource.getLocation.getPath
  }

  extension (groupId: String) {
    def %(artifactId: String): coursier.core.Module =
      coursier.core.Module(
        coursier.core.Organization(groupId),
        coursier.core.ModuleName(artifactId),
        Map.empty
      )

    def %%(artifactId: String): coursier.core.Module =
      %(artifactId + "_3")
  }

  extension (module: coursier.core.Module) {
    def %(version: String): coursier.core.Dependency =
      coursier.core.Dependency(module, version)
  }

  def main(args: Array[String]): Unit = {
    val jarNames = args match {
      case Array(groupId, artifactId, version) =>
        (groupId %% artifactId % version) :: Nil
      case Array(groupId, artifactId) =>
        (groupId %% artifactId % "latest.release") :: Nil
      case _ =>
        List(
          "org.scalikejdbc" %% "scalikejdbc-core" % "4.0.0",
          "commons-dbcp" % "commons-dbcp" % "1.4",
          "com.jolbox" % "bonecp" % "0.8.0.RELEASE",
        )
    }
    val csResult = coursier.Fetch().addDependencies(jarNames: _*).runResult()
    val pomList = csResult.artifacts.flatMap(_._1.extra.map(_._2)).map(_.url).filter(_.endsWith(".pom"))
    pomList.sorted.foreach(println)
    val jars = csResult.files
    println("*" * 100)
    println("pom count = " + pomList.size + ", jar count = " + jars.size)
    jars.map(_.toString.split("/maven2/").last).sorted.foreach(println)
    println("*" * 100)
    jars
      .lazyZip(pomList)
      .filter { (jar, pom) =>
        pom.contains("_3/") && pom.contains("_3-")
      }
      .filterNot { (jar, pom) =>
        pom.contains("/scala3-library_3/")
      }
      .foreach { (jar, pom) =>
        println("start inspect " + jar)
        val result = run(
          traverser = {
            import org.wartremover.warts.*
            List[WartTraverser](
              ArrayEquals,
              CollectHeadOption,
              DropTakeToSlice,
              FilterHeadOption,
              FilterSize,
              GetGetOrElse,
              GetOrElseNull,
              ListAppend,
              ListUnapply,
              RedundantConversions,
              ReverseFind,
              ReverseTakeReverse,
              ScalaApp,
              SizeIs,
              SortFilter,
              ThreadSleep,
            ).reduceLeft(_ compose _)
          },
          jars = jar.getAbsolutePath :: Nil,
          dependenciesClasspath = jars.map(_.getAbsolutePath).toList,
          githubUrl = {
            val pomXml = scala.xml.XML.load(new java.net.URL(pom))
            val baseOpt = Option((pomXml \ "scm" \ "url").text).filter(_.trim.nonEmpty).map {
              case s"${other}github.com:${user}/${repo}.git" =>
                s"https://github.com/${user}/${repo}/blob/"
              case s"${other}github.com/${user}/${repo}" =>
                s"https://github.com/${user}/${repo}/blob/"
            }
            baseOpt.map {
              _ + Option((pomXml \ "version").text).filter(_.trim.nonEmpty).map("v" + _).getOrElse("main")
            }
          }
        )
        println("end inspect " + jar)
        println("*" * 100)
        result.foreach(println)
      }
  }

  case class Result(message: String, line: Int, path: String)

  def run(
    traverser: WartTraverser,
    jars: List[String],
    dependenciesClasspath: List[String],
    githubUrl: Option[String]
  ): List[Result] = {
    val results = List.newBuilder[Result]
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        val universe: WartUniverse.Aux[q.type] = new WartUniverse(onlyWarning = false, logLevel = LogLevel.Debug) {
          override type Q = q.type
          override val quotes: q.type = q
          override def onError(msg: String, pos: quotes.reflect.Position): Unit = {
            super.onError(msg, pos)
            val startLine = pos.startLine + 1
            val endLine = pos.endLine + 1
            results += Result(
              message = msg,
              line = startLine,
              path = pos.sourceFile.path
            )
            githubUrl.foreach { url =>
              val line = if (startLine == endLine) {
                "#L" + startLine
              } else {
                "#L" + startLine + "-L" + endLine
              }
              println(url + "/" + pos.sourceFile.path + line)
            }
            println(
              Seq[(String, String | Int)](
                "message" -> msg,
                "path" -> pos.sourceFile.path,
                "startLine" -> startLine,
                "endLine" -> endLine,
                "startColumn" -> pos.startColumn,
                "endColumn" -> pos.endColumn
              ).map { (k, v) => s"$k = $v" }.mkString(", ")
            )
          }
        }
        val treeTraverser = traverser.apply(universe)
        val count = tastys.size
        val x = math.max(count / 30, 1).toInt
        println("tasty file count = " + count)
        tastys.zipWithIndex.foreach { (tasty, i) =>
          if (i % x == 0) {
            println(s"[${timeString()}] inspecting $i / $count")
          }
          // compiler crash if remove explicit `Tree` type
          // https://github.com/lampepfl/dotty/issues/14785
          val tree: Tree = tasty.ast
          try {
            treeTraverser.traverseTree(tree)(tree.symbol)
          } catch {
            case e: Throwable =>
              println((tasty.path, e))
              throw e
          }
        }
      }
    }
    TastyInspector.inspectAllTastyFiles(
      tastyFiles = Nil,
      jars = jars,
      dependenciesClasspath = dependenciesClasspath,
    )(inspector)
    results.result()
  }
}
