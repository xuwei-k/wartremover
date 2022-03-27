package org.wartremover

import scala.quoted.Quotes
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import scala.reflect.ClassTag

object WartInspector {
  def jarPathFromType[A](implicit c: ClassTag[A]): String = {
    c.runtimeClass.getProtectionDomain.getCodeSource.getLocation.getPath
  }

  def jarPathFromCoursierCache(groupId: String, artifactId: String, version: String): String = {
    List(
      scala.util.Properties.userHome,
      "Library/Caches/Coursier/v1/https/repo1.maven.org/maven2",
      groupId.replace('.', '/'),
      artifactId + "_3",
      version,
      artifactId + "_3-" + version + ".jar"
    ).mkString("/")
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
    /*
    val jarNames = List(
      "ast",
      "core",
      "ext",
      "jackson-core",
      "jackson",
      "mongo",
      "native-core",
      "native",
      "scalap",
      "scalaz",
      "xml"
    ).map { x =>
      coursier.core.Dependency(
        coursier.core.Module(
          coursier.core.Organization("org.json4s"),
          coursier.core.ModuleName(s"json4s-${x}_3"),
          Map.empty
        ),
        "4.1.0-M1"
      )
    }
     */

    val jarNames = args match {
      case Array(groupId, artifactId, version) =>
        (groupId %% artifactId % version) :: Nil
      case _ =>
        List(
          "org.scalikejdbc" %% "scalikejdbc-core" % "4.0.0",
          "commons-dbcp" % "commons-dbcp" % "1.4",
          "com.jolbox" % "bonecp" % "0.8.0.RELEASE",
        )
    }
    val jars = coursier.Fetch().addDependencies(jarNames: _*).run()
    jars.map(_.toString.split("/maven2/").last).foreach(println)
    println("*" * 100)
    val result = run(
      traverser = List[WartTraverser](
        org.wartremover.warts.CollectHeadOption,
        org.wartremover.warts.DropTakeToSlice,
        org.wartremover.warts.FilterHeadOption,
        org.wartremover.warts.FilterSize,
        org.wartremover.warts.GetGetOrElse,
        org.wartremover.warts.GetOrElseNull,
        org.wartremover.warts.ReverseFind,
        org.wartremover.warts.ReverseTakeReverse,
        org.wartremover.warts.SizeIs,
        org.wartremover.warts.SortFilter,
      ).reduceLeft(_ compose _),
      jars = jars.map(_.getAbsolutePath).toList,
      dependenciesClasspath = Nil
    )
    println("*" * 100)
    result.foreach(println)
  }

  case class Result(message: String, line: Int, path: String)

  def run(traverser: WartTraverser, jars: List[String], dependenciesClasspath: List[String]): List[Result] = {
    val results = List.newBuilder[Result]
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        val universe: WartUniverse.Aux[q.type] = new WartUniverse(onlyWarning = false, logLevel = LogLevel.Debug) {
          override type Q = q.type
          override val quotes: q.type = q
          override def onError(msg: String, pos: quotes.reflect.Position): Unit = {
            super.onError(msg, pos)
            results += Result(
              message = msg,
              line = pos.startLine,
              path = pos.sourceFile.path
            )
            println(
              Seq[(String, String | Int)](
                "message" -> msg,
                "path" -> pos.sourceFile.path,
                "startLine" -> pos.startLine,
                "endLine" -> pos.endLine,
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
            println(s"inspecting $i / $count")
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
