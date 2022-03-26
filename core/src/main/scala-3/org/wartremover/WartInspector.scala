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

  def main(args: Array[String]): Unit = {
    import coursier._
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
    val jars = Fetch().addDependencies(jarNames: _*).run()
    jars.foreach(println)
    run(
      traverser = List[WartTraverser](
        org.wartremover.warts.CollectHeadOption,
        org.wartremover.warts.SizeIs,
        org.wartremover.warts.FilterHeadOption,
        org.wartremover.warts.SortFilter,
      ).reduceLeft(_ compose _),
      jars = jars.map(_.getAbsolutePath).toList,
      dependenciesClasspath = Nil
    )
  }

  case class Result(message: String, line: Int, path: String)

  def run(traverser: WartTraverser, jars: List[String], dependenciesClasspath: List[String]): List[Result] = {
    val results = List.newBuilder[Result]
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        val universe: WartUniverse.Aux[q.type] = WartUniverse.withReporter(
          onlyWarning = true,
          logLevel = LogLevel.Info,
          quotes = q,
          error = (msg, pos) => {
            results += Result(
              message = msg,
              line = pos.startLine,
              path = pos.sourceFile.path
            )
            println(
              Seq[(String, Any)](
                "message" -> msg,
                "file" -> pos.sourceFile,
                "startLine" -> pos.startLine,
                "endLine" -> pos.endLine,
                "startColumn" -> pos.startColumn,
                "endColumn" -> pos.endColumn
              ).map { (k, v) => s"$k = $v" }.mkString(", ")
            )
          },
          warn = (msg, pos) => println((msg, pos)),
        )
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
          treeTraverser.traverseTree(tree)(tree.symbol)
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
