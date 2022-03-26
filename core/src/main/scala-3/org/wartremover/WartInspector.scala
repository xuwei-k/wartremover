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
    val reporter = new test.WartReporter()
    val inspector = new Inspector {
      def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        import q.reflect.*
        val universe: WartUniverse.Aux[q.type] = WartUniverse.withReporter(
          onlyWarning = true,
          logLevel = LogLevel.Info,
          quotes = q,
          error = (msg, pos) => println((msg, pos)),
          warn = (msg, pos) => println((msg, pos)),
        )
        val traverser = List[WartTraverser](
//          org.wartremover.warts.CollectHeadOption,
          org.wartremover.warts.SizeIs,
          //         org.wartremover.warts.FilterHeadOption,
          //        org.wartremover.warts.SortFilter,
        ).reduceLeft(_ compose _).apply(universe)
        val count = tastys.size
        val x = math.max(count / 30, 1).toInt
        println("tasty file count = " + count)
        tastys.zipWithIndex.foreach { (tasty, i) =>
          if (i % x == 0) {
            println(s"inspecting $i / $count")
          }

          // compiler crash if remove explicit `Tree` type
          // TODO report bug?
          val tree: Tree = tasty.ast
          traverser.traverseTree(tree)(tree.symbol)
        }
      }
    }
    val classpath = System.getProperty("java.class.path").split(':').toList
    classpath.sorted.foreach(println)
    TastyInspector.inspectAllTastyFiles(
      tastyFiles = Nil,
      jars = jarPathFromType[dotty.tools.dotc.Compiler] :: Nil,
      dependenciesClasspath = classpath
    )(inspector)
    val result = reporter.result
    println(result.size)
    result.foreach(println)
  }
}
