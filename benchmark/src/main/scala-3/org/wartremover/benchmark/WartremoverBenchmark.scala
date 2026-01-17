package org.wartremover.benchmark

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.TearDown
import org.wartremover.LogLevel.Disable
import scala.quoted.Quotes
import scala.tasty.inspector.Inspector
import scala.tasty.inspector.Tasty
import scala.tasty.inspector.TastyInspector
import java.io.File
import sbt.io.IO
import scala.io.Source
import scala.sys.process.Process

object WartremoverBenchmark {
  val compilerJarPath: String =
    dotty.tools.dotc.Main.getClass.getProtectionDomain.getCodeSource.getLocation.toURI.toURL.getFile

  final case class Repo(githubUser: String, githubName: String, ref: String) {
    def cloneTo(dir: File): Unit = {
      val args = Seq[String](
        "git",
        "-c",
        "advice.detachedHead=false",
        "clone",
        s"https://github.com/${githubUser}/${githubName}.git",
        "-b",
        ref,
        "--depth",
        "1",
        dir.getAbsolutePath
      )
      println(s"run '${args.mkString(" ")}'")
      val res = Process(args).!
      assert(res == 0, res)
    }
  }
}

@State(Scope.Benchmark)
abstract class WartremoverBenchmark {
  protected def wart: org.wartremover.WartTraverser

  private val dir = new File(".")

  private var tastyFiles: List[String] = Nil

  @TearDown
  def tearDown(): Unit = {
    IO.delete(dir)
  }

  @Setup
  def setup(): Unit = {
    IO.delete(dir)
    dir.mkdirs()
    import sbt.io.syntax.*
    val repo = WartremoverBenchmark.Repo("scala", "scala3", "3.7.4")
    repo.cloneTo(dir)
    tastyFiles =
      IO.unzip(new File(WartremoverBenchmark.compilerJarPath), dir, _.endsWith(".tasty")).map(_.getCanonicalPath).toList
    println("tasty files = " + tastyFiles.size)
  }

  @Benchmark
  def test(): Int = {
    var count = 0
    val inspector: Inspector = new Inspector {
      override def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
        val universe = new org.wartremover.WartUniverse(true, Disable) {
          override type Q = q.type
          override val quotes: Q = q
          override protected def onWarn(msg: String, pos: quotes.reflect.Position): Unit = {
            count += 1
          }
        }
        val traverser = wart.apply(universe)
        tastys.foreach { t =>
          traverser.traverseTree(t.ast)(q.reflect.Symbol.spliceOwner)
        }
      }
    }
    TastyInspector.inspectAllTastyFiles(
      tastyFiles = tastyFiles,
      jars = Nil,
      dependenciesClasspath = Nil
    )(inspector)
    count
  }
}
